use std::error::Error;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, mpsc};
use std::thread;
use std::time::Duration;

use cdr::{CdrLe, Infinite, deserialize, serialize};
use serde::{Deserialize, Serialize};
use zenoh::{Config, Wait};

pub type Result<T> = std::result::Result<T, SerializeError>;

fn main() -> Result<()> {
    println!("Starting serialization test");
    zenoh::init_log_from_env_or("error");

    const PUBLISH_TOPIC: &str = "blueberry/person/default";
    const SUBSCRIBE_TOPIC: &str = "blueberry/person/expected";

    let expected_person = Person::default();
    let expected_bytes = serialize_person(&expected_person)?;

    let session = zenoh::open(Config::default())
        .wait()
        .map_err(SerializeError::Zenoh)?;

    let publisher = session
        .declare_publisher(PUBLISH_TOPIC)
        .wait()
        .map_err(SerializeError::Zenoh)?;
    let subscriber = session
        .declare_subscriber(SUBSCRIBE_TOPIC)
        .wait()
        .map_err(SerializeError::Zenoh)?;

    let stop = Arc::new(AtomicBool::new(false));
    let (tx, rx) = mpsc::channel();

    {
        let stop = Arc::clone(&stop);
        let tx = tx.clone();
        let bytes = expected_bytes.clone();
        thread::spawn(move || {
            while !stop.load(Ordering::Relaxed) {
                if let Err(err) = publisher.put(bytes.clone()).wait() {
                    let _ = tx.send(Err(SerializeError::Zenoh(err)));
                    stop.store(true, Ordering::Relaxed);
                    break;
                }
                thread::sleep(Duration::from_secs(1));
            }
        });
    }

    {
        let stop = Arc::clone(&stop);
        let tx = tx.clone();
        let expected = expected_person.clone();
        thread::spawn(move || {
            loop {
                if stop.load(Ordering::Relaxed) {
                    break;
                }
                match subscriber.recv() {
                    Ok(sample) => {
                        let bytes = sample.payload().to_bytes();
                        let result = match deserialize::<Person>(&bytes) {
                            Ok(person) => {
                                if person == expected {
                                    Ok(())
                                } else {
                                    Err(SerializeError::Mismatch(person))
                                }
                            }
                            Err(err) => Err(SerializeError::Cdr(err)),
                        };
                        let _ = tx.send(result);
                        stop.store(true, Ordering::Relaxed);
                        break;
                    }
                    Err(err) => {
                        let _ = tx.send(Err(SerializeError::Zenoh(err)));
                        stop.store(true, Ordering::Relaxed);
                        break;
                    }
                }
            }
        });
    }

    match rx.recv_timeout(Duration::from_secs(60)) {
        Ok(Ok(())) => Ok(()),
        Ok(Err(err)) => Err(err),
        Err(_) => Err(SerializeError::Timeout),
    }
}

/// Representation of the `Person` message described in the example IDL.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Person {
    pub name: String,
    pub age: i32,
    pub is_active: bool,
    pub codename: String,
    pub readings: [i32; 8],
}

impl Default for Person {
    fn default() -> Self {
        Self {
            name: "Potato".into(),
            age: 0x1234,
            is_active: true,
            codename: "maisquenada".into(),
            readings: [0, 1, 2, 3, 4, 0xffff, 6, 7],
        }
    }
}

/// Serializes a `Person` using CDR little-endian encoding with an encapsulation
/// header, honoring field alignment and boolean-as-byte rules.
pub fn serialize_person(person: &Person) -> Result<Vec<u8>> {
    const CODENAME_BOUND: usize = 16;
    let codename_len = person.codename.as_bytes().len();
    if codename_len > CODENAME_BOUND {
        return Err(SerializeError::StringTooLong {
            field: "codename",
            max: CODENAME_BOUND,
            len: codename_len,
        });
    }

    serialize::<_, _, CdrLe>(person, Infinite).map_err(SerializeError::Cdr)
}

/// Deserializes a `Person` using CDR little-endian encoding with an encapsulation header.
pub fn deserialize_person(bytes: &[u8]) -> Result<Person> {
    deserialize(bytes).map_err(SerializeError::Cdr)
}

#[derive(Debug)]
pub enum SerializeError {
    StringTooLong {
        field: &'static str,
        max: usize,
        len: usize,
    },
    Cdr(cdr::Error),
    Zenoh(zenoh::Error),
    Mismatch(Person),
    Timeout,
}

impl fmt::Display for SerializeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SerializeError::StringTooLong { field, max, len } => {
                write!(f, "string field `{field}` too long: {len} > {max}")
            }
            SerializeError::Cdr(err) => write!(f, "cdr serialization failed: {err}"),
            SerializeError::Zenoh(err) => write!(f, "zenoh error: {err}"),
            SerializeError::Mismatch(person) => {
                write!(f, "received unexpected person payload: {person:?}")
            }
            SerializeError::Timeout => write!(f, "no message received within 60s"),
        }
    }
}

impl Error for SerializeError {}

#[cfg(test)]
mod tests {
    use super::*;

    const PERSON_DEFAULT_BYTES: [u8; 72] = [
        0x00, 0x01, 0x00, 0x00, // CDR little-endian encapsulation header
        0x07, 0x00, 0x00, 0x00, // name length (includes null)
        0x50, 0x6f, 0x74, 0x61, 0x74, 0x6f, 0x00, // "Potato\0"
        0x00, // padding to 4-byte alignment
        0x34, 0x12, 0x00, 0x00, // age = 0x1234
        0x01, // is_active = true (byte-encoded bool)
        0x00, 0x00, 0x00, // padding before codename (align to 4)
        0x0c, 0x00, 0x00, 0x00, // codename length (includes null)
        0x6d, 0x61, 0x69, 0x73, 0x71, 0x75, 0x65, 0x6e, 0x61, 0x64, 0x61,
        0x00, // "maisquenada\0"
        0x00, 0x00, 0x00, 0x00, // readings[0] = 0
        0x01, 0x00, 0x00, 0x00, // readings[1] = 1
        0x02, 0x00, 0x00, 0x00, // readings[2] = 2
        0x03, 0x00, 0x00, 0x00, // readings[3] = 3
        0x04, 0x00, 0x00, 0x00, // readings[4] = 4
        0xff, 0xff, 0x00, 0x00, // readings[5] = 0xffff
        0x06, 0x00, 0x00, 0x00, // readings[6] = 6
        0x07, 0x00, 0x00, 0x00, // readings[7] = 7
    ];

    #[test]
    fn serializes_default_person() {
        let bytes = serialize_person(&Person::default()).expect("serialize default person");
        assert_eq!(bytes, PERSON_DEFAULT_BYTES);
    }

    #[test]
    fn rejects_codename_overflow() {
        let mut person = Person::default();
        person.codename = "this-is-way-too-long".into();
        let err = serialize_person(&person).unwrap_err();
        assert!(matches!(err, SerializeError::StringTooLong { .. }));
    }
}
