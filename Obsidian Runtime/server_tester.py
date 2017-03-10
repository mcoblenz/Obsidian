import json
import socket

def send_json(port, meth, fn=None, args=[]):
    fn_str = fn
    if fn_str == None:
        fn_str = ""

    json_dump = json.dumps(
        {
        "jsonrpc": "2.0",
        "method": "deploy",
        "params": {
            "type": 1,
            "chaincodeID":{
                "path":"github.com/hyperledger/fabric/examples/chaincode/go/chaincode_example02"
            },
            "ctorMsg": {
                "function":fn_str,
                "args":args
            }
        },
        "id": 1
        }
    )

    sk = socket.socket()
    sk.connect(('127.0.0.1', port))
    sk.send(json_dump)
    sk.close()
