import requests
import json

def main():
    url = 'http://localhost:3003/bss'

    headers = {'content-type': 'application/json', 'accept': 'application/json'}

    result = {
        "jsonrpc": "2.0",
        "method": "bss.cls.order_create",
        "params": {"contractid": "234567","channel": "telesales","salesman": "user123","created-at": "2018-04-01","brand": "yoigo","customer-account": {"custnum": 6600013,"billing-accounts": [{"name":"foo","iban":"ES1234567898012","mandate": "QWERTYUIIOP","delivery-method":"e-invoice"}]},"payment":{"method":"creditcard","ccreference":"12345678900987","ccvalid":"102020","authnumber":"125","binnumber":"23445"},"order-inspection":{"result":"ok","description":"some_description - customerSegment: CONSUMER","level":"0","rule-id":"some_terminal_rule","risk-code":"","delivery-secure":1},"items":[{"type":"main","product-offering-id":"MAINOFFERING123","items":[{"type":"mobile-subscription","product-offering-id":"POSTPAIDSIMOFFERING123","product-id":"CONT15","attributes":{"number-type":"new","number":"622612988"},"billing-account-index":0},{"type": "sim","product-offering-id": "SIM10123","product-id": "SIM","attributes":{},"shipping-address":{"title": "Mr.","fname":"Bruce Son1","lname": "Wayne","lname2":"Sanchez","nationality":"ES","id-type":"NIF","person-id":"12223423H","street":"Penny Rito 562","city":"gotham","region":"gotham-metro","zip":"28028","country":"us","email":"bruce@evilcorp.io","sms_number":"666666666","phone_number":"966666666","address-validation-data":{"city_code":"0123","street_code":"0124","municipality_code": "0125"}}}]}]},
        "id": 1,
    }

    response = requests.post(
       url, data=json.dumps(result), headers=headers)

    print response.status_code
    print response.text
    data = response.json()

if __name__ == "__main__":
    main()
