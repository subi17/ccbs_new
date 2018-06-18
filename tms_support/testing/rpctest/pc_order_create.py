import requests
import json
import sys

def instruction():
   print "Example: python bss_order_create_pc.py inputorder.json"
   sys.exit()

if len(sys.argv) < 1: 
    instruction()
 
def main(args):
    url = 'http://localhost:3003/bss'

    headers = {'content-type': 'application/json', 'accept': 'application/json'}

    result = {
        "jsonrpc": "2.0",
        "method": "bss.cls.order_create",
        "params": args,
        "id": 1,
    }

    response = requests.post(
       url, data=json.dumps(result), headers=headers)

    print response.status_code
    print response.text
    data = response.json()

if __name__ == "__main__":
    json_data = json.load(open("./bssjson/" + sys.argv[1]))
    main(json_data)

