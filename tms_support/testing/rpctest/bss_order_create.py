import requests
import json

def main():
    url = 'http://localhost:3003/bss'

    headers = {'content-type': 'application/json', 'accept': 'application/json'}

    result = {
        "jsonrpc": "2.0",
        "method": "bss.cls.order_create",
        "params": {"contractid":"123456","channel":"customer-care","salesman":"user123","created-at":"2018-04-01","brand":"yoigo","agreement-customer":{"custnum": 6600007 },"items":[{"type":"acc","subtype":"existing-customer","product-offering-id":"ACC-EXISTING","product-id":"ACC-EXISTING","attributes":{"memo":{"title":"Agreement customer change","content":"Customer request"},"subscription-id": 12647460,"execution-date":"2018-04-01","custnum": 6600021}}]},
        "id": 1,
    }

    response = requests.post(
       url, data=json.dumps(result), headers=headers)

    print response.status_code
    print response.text
    data = response.json()

if __name__ == "__main__":
    main()
