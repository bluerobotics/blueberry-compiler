#[allow(
    dead_code,
    non_snake_case,
    non_camel_case_types,
    unused_imports
)]
#[path = "../../../rust/mod.rs"]
mod blueberry;

use std::net::UdpSocket;
use std::time::{Duration, Instant};

use blueberry::dictionary::devices::{IdMessage, VersionMessage, WhosThereMessage};
use blueberry::{request_packet, Message};

const DEVICE_ADDR: &str = "192.168.31.28";
const BLUEBERRY_PORT: u16 = 16962;
const RECV_TIMEOUT: Duration = Duration::from_secs(5);

fn to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn main() {
    let addr = format!("{DEVICE_ADDR}:{BLUEBERRY_PORT}");

    let sock = UdpSocket::bind(("0.0.0.0", BLUEBERRY_PORT)).expect("Failed to bind UDP socket");
    sock.connect(&addr).expect("Failed to connect to device");
    sock.set_read_timeout(Some(Duration::from_millis(100)))
        .expect("Failed to set read timeout");

    println!("INFO: Communicating with {addr} via UDP");

    let packets = [
        request_packet::<IdMessage>(),
        request_packet::<VersionMessage>(),
        request_packet::<WhosThereMessage>(),
    ];

    for pkt in &packets {
        println!("TX packet ({} bytes): {:?}", pkt.len(), pkt);
        sock.send(pkt).expect("UDP send failed");
    }

    println!(
        "INFO: Waiting for responses ({:.0}s timeout)...",
        RECV_TIMEOUT.as_secs_f64()
    );

    let mut buf = Vec::new();
    let mut recv_buf = [0u8; 4096];
    let deadline = Instant::now() + RECV_TIMEOUT;

    loop {
        if Instant::now() >= deadline {
            println!("INFO: Receive finished (timeout)");
            break;
        }
        match sock.recv(&mut recv_buf) {
            Ok(n) => {
                buf.extend_from_slice(&recv_buf[..n]);
                for msg in Message::extract_packets(&mut buf) {
                    println!("INFO:   {msg}");
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => continue,
            Err(e) if e.kind() == std::io::ErrorKind::TimedOut => continue,
            Err(e) => {
                eprintln!("ERROR: UDP recv: {e}");
                break;
            }
        }
    }
}
