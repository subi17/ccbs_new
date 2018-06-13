import requests
import json

def main():
    url = 'http://localhost:3003/bss'

    headers = {'content-type': 'application/json', 'accept': 'application/json'}

    result = {
        "jsonrpc": "2.0",
        "method": "bss.cls.order_create",
        "params": {     "contractid": "123456",     "channel": "pos",   "salesman": "user123",  "created-at": "2018-04-01",     "brand": "yoigo",   "inspection": {         "result": "ok",         "description": "some_description _ customerSegment: CONSUMER",      "level": "0",       "rule_id": "some_terminal_rule",        "risk_code": "",        "delivery_secure": True     },  "addresses": [{         "address_id": 1,        "street": "Penny lane",         "street_type": "Narrow",        "building_number": 24,      "block": "A",       "floor": "first",       "stair": "",        "door": "2",        "letter": "C",      "km": "20",         "hand": "",         "bis_duplicate": "",        "additional_address": "",       "normalizedid": "",         "latitude": "62.242603",        "longitude": "25.747257",       "city": "gotham",       "region": "01",         "zip": "28028",         "country": "us",        "gescal": "",       "coveragetoken": "",        "validation_data": {            "city_code": "0123",            "street_code": "0124",          "municipality_code": "0125"         }   },  {       "address_id": 2,        "street": "Penny lane 234325",      "street_type": "Narrow",        "building_number": 24,      "block": "A",       "floor": "first",       "stair": "",        "door": "2",        "letter": "C",      "km": "20",         "hand": "",         "bis_duplicate": "",        "additional_address": "",       "normalizedid": "",         "latitude": "62.242603",        "longitude": "25.747257",       "city": "gotham",       "region": "01",         "zip": "28028",         "country": "us",        "gescal": "",       "coveragetoken": "",        "validation_data": {            "city_code": "0123",            "street_code": "0124",          "municipality_code": "0125"         }   }],     "customer": {       "title": "Mr.",         "fname": "Bruce",       "lname": "Wayne",       "lname2": "Sanchez",        "profession": "Customer representative",        "nationality": "ES",        "language": "es",       "person_id": "B1231232H",       "id_type": "NIF",       "self_employed": False,         "birthday": "1971-01-01",       "foundation_date": "1991-01-01",        "email": "bruce@evilcorp.io",       "sms_number": "666666666",      "phone_number": "966666666",        "default_address_data_index": 1,        "marketing": {          "email": False,             "post": False,          "sms": False,           "bank_3rd": False,          "email_3rd": False,             "post_3rd": False,          "sms_3rd": False,           "dont_share_personal_data": False       },      "customer_accounts": [{             "default_account": True,            "account_name": "First",            "valid_from": "2018-04-01",             "valid_to": "2018-04-01",           "billing_address_data_index": 1,            "billing_accounts": [{              "invoice_cycle": 10,                "invoice_interval": 3,              "invoice_group": "VAT1",                "invoice_delivery_type": "e_invoice",               "invoice_charge_type": "DirectDebit",               "invoice_shipping_address_data_index": 2,               "bank_account": "ES141720101010101010101044",               "bank_name": "Santander",               "currency": "eur",              "mandate_id": "QERERTTE",               "mandate_date": "2018-04-01",               "valid_from": "2018-04-01",                 "valid_to": "2018-04-01"            }]      }]  },  "items": [{         "type": "main",         "product-offering-id": "MAINOFFERING123",       "items": [{             "type": "mobile_subscription",          "product-offering-id": "POSTPAIDSIMOFFERING123",            "product-id": "CONT15",             "attributes": {                 "number_type": "new",               "number": "675684341"           },          "billing_account_index": 1      },      {           "type": "sim",          "product-offering-id": "SIM10123",          "product-id": "SIM",            "attributes": {                 "ICC": "2343456456456456",              "shipping_address_data_index": 2            }       }]  }],     "payment_method": {         "method": "creditcard",         "ccreference": "12345678900987",      "authnumber": "125",        "binnumber": "23445"    } },
        "id": 1,
    }

    response = requests.post(
       url, data=json.dumps(result), headers=headers)

    print response.status_code
    print response.text
    data = response.json()

if __name__ == "__main__":
    main()
