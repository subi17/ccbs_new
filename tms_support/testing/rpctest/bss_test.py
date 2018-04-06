import requests
import json

def main():
    url = 'http://localhost:3003/bss'

    headers = {'content-type': 'application/json', 'accept': 'application/json'}

    result = {
        "jsonrpc": "2.0",
        "method": "bss.cls.Test",
        "params": [{"serviceRef": "1234", "fromDate": "2017-01-01 00:00:00", "toDate": "2017-01-31 23:59:59"}],
        "id": 0,
    }

    response = requests.post(
       url, data=json.dumps(result), headers=headers)

    print response.status_code
    print response.text
    data = response.json()

if __name__ == "__main__":
    main()
