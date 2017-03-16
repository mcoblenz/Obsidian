import json
import socket
import sys

# this script requires python3

def toBigIntSerial(val):
    return (val.to_bytes(32, byteorder='big', signed=True)).hex()

def fromBigIntSerial(hexStr):
    return int.from_bytes(bytes.fromhex(hexStr), byteorder='big', signed=True)

# all received values are assumed to be integers
def send_json(port, meth, fn, args=[]):
    json_dump = json.dumps(
        {
        "jsonrpc": "2.0",
        "method": meth,
        "params": {
            "type": 1,
            "chaincodeID":{
                "path":"github.com/hyperledger/fabric/examples/chaincode/go/chaincode_example02"
            },
            "ctorMsg": {
                "function":fn,
                "args":args
            }
        },
        "id": 1
        }
    )

    sk = socket.socket()
    sk.connect(('127.0.0.1', port))
    sk.send(json_dump.encode())

    raw_recv = []
    
    while len(raw_recv) == 0:
        raw_recv = sk.recv(4096)

    print(raw_recv)
    
    received_json = json.loads(raw_recv)

    print("Received JSON:")
    print(received_json)

    print("Return Integer:")
    print(fromBigIntSerial(received_json['result']['message']))
    sk.close()

# all args are assumed to be integers
if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage: python server_tester.py [port] [deploy/query/invoke] [txName] [arg_0] ... [arg_n]")
        exit(0)
    port = int(sys.argv[1])
    meth = sys.argv[2]
    fn = sys.argv[3]

    # assume that all args are big ints
    args = list(map(lambda x : toBigIntSerial(int(x)), sys.argv[4:]))
    send_json(port, meth, fn, args)      
