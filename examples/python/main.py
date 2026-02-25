"""Blueberry protocol test — communicate with a device over UDP."""

import logging
import socket
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / ".." / ".." / "python"))

from blueberry_serde import BLUEBERRY_PORT
from dictionary.devices.id_message import IdMessage
from dictionary.devices.version_message import VersionMessage
from dictionary.devices.whos_there_message import WhosThereMessage
from message import Message

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
log = logging.getLogger(__name__)

DEVICE_ADDR = ("192.168.31.28", BLUEBERRY_PORT)
RECV_TIMEOUT = 1.0

MESSAGES_TO_REQUEST = [IdMessage, VersionMessage, WhosThereMessage]


def main() -> None:
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(("0.0.0.0", BLUEBERRY_PORT))
    sock.settimeout(RECV_TIMEOUT)

    log.info("Communicating with %s:%d via UDP", *DEVICE_ADDR)

    for msg_cls in MESSAGES_TO_REQUEST:
        pkt = Message.request_packet_for(msg_cls)
        sock.sendto(pkt, DEVICE_ADDR)
        log.info(
            "Sent %s request (%d bytes): %s",
            msg_cls.__name__,
            len(pkt),
            list(pkt),
        )

    log.info("Waiting for responses (%.0fs timeout)...", RECV_TIMEOUT)
    buf = bytearray()

    try:
        while True:
            data, _addr = sock.recvfrom(4096)
            if not data:
                continue
            buf.extend(data)
            for msg in Message.extract_packets(buf):
                log.info("  %s", msg)
    except TimeoutError:
        log.info("Receive finished (timeout)")
    finally:
        sock.close()


if __name__ == "__main__":
    main()
