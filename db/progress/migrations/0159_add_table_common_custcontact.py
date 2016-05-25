from gearbox.migrations import Migration

class AddTableCustContact(Migration):

    database = "common"

    def up(self):
        t = self.table('CustContact', area="Sta_Data_32", label="CustContact", table_trigger=[{'crc': '?', 'procedure': 'rd-custcontact.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-custcontact.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="custcontact", desc="CustContact")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="CustContact", column_label="CustContact", position=2, order=10, help="CustContact's number")
        t.column('CustType', 'integer', mandatory=True, format=">>9", initial="0", max_width=4, label="CustType", column_label="CustType", position=3, order=20, help="Contact customer type")
        t.column('CustName', 'character', format="x(30)", initial="", max_width=60, label="CustContact's name", column_label="CustContact's name", position=4, order=30, help="CustContact's name")
        t.column('Address', 'character', format="x(30)", initial="", max_width=60, label="Address", column_label="Address", position=5, order=40, help="CustContact's mailing address (street, p.o. box)")
        t.column('ZipCode', 'character', format="x(8)", initial="", max_width=16, label="Postal code", column_label="Postcd", position=6, order=50, help="CustContact's postal code")
        t.column('PostOffice', 'character', format="x(24)", initial="", max_width=48, label="Postal Addr.", column_label="Postaddr", position=7, order=60, help="CustContact's postal address")
        t.column('Country', 'character', format="x(3)", initial="", max_width=6, label="Country", column_label="Country", position=8, order=70, help="CustContact's country code")
        t.column('Language', 'integer', valexp="Language > 0", format=">>9", initial="0", max_width=4, label="Language", column_label="Language", position=9, order=80, valmsg="Must be 1 ... 9 !", help="CustContact's language code (1 ...999)")
        t.column('OrgId', 'character', format="x(11)", initial="", max_width=22, label="Pers/Comp.ID", position=10, order=90, help="CustContact's organisation ID or personal ID")
        t.column('Email', 'character', format="x(60)", initial="", max_width=120, label="Email", column_label="Email", position=11, order=100, help="CustContact's Email ID")
        t.column('HonTitle', 'character', format="x(16)", initial="", max_width=32, label="Title", column_label="Title", position=12, order=120, help="CustContact honorary title (printed f.ex. into invoice)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=13, order=140, help="Code Of Brand")
        t.column('SMSNumber', 'character', format="x(15)", initial="", max_width=30, label="SMS Number", column_label="SMS", position=14, order=160, help="Mobile number for SMS messages")
        t.column('FirstName', 'character', format="x(20)", initial="", max_width=40, label="Forename", column_label="Forename", position=15, order=170, help="CustContact's forename")
        t.column('DirMarkSMS', 'logical', format="yes/no", initial="no", max_width=1, label="Direct Marketing Via SMS", column_label="Dir.Mark.SMS", position=16, order=180, help="Direct marketing using SMS")
        t.column('DirMarkEmail', 'logical', format="yes/no", initial="no", max_width=1, label="Direct Marketing Via Email", column_label="Dir.Mark.Email", position=17, order=190, help="Direct marketing using Email")
        t.column('Phone', 'character', format="x(16)", initial="", max_width=32, label="PhoneNo", column_label="PhoneNo", position=18, order=300, help="CustContact's phone number")
        t.column('OutMarkPost', 'logical', format="yes/no", initial="no", max_width=1, label="Out. Marketing Via Post", column_label="Out.Mark.Post", position=19, order=210, help="3rd part marketing using post")
        t.column('OutMarkSMS', 'logical', format="yes/no", initial="no", max_width=1, label="Out. Marketing Via SMS", column_label="Out.Mark.SMS", position=20, order=220, help="3rd party marketing using SMS")
        t.column('OutMarkEmail', 'logical', format="yes/no", initial="no", max_width=1, label="Out. Marketing Via Email", column_label="Out.Mark.Email", position=21, order=221, help="3rd party marketing using eMail")
        t.column('CustIdType', 'character', format="x(8)", initial="", max_width=16, label="CustContact ID Type", column_label="ID Type", position=22, order=230, help="CustContact ID type")
        t.column('Nationality', 'character', format="x(8)", initial="", max_width=16, label="Nationality", column_label="Nation.", position=23, order=240, help="Nationality")
        t.column('SurName2', 'character', format="x(30)", initial="", max_width=60, label="Second Surname", column_label="2.Surname", position=24, order=250, help="Second surname")
        t.column('Region', 'character', format="x(8)", initial="", max_width=16, label="Region", position=25, order=260, help="Region code")
        t.column('AddressCodC', 'character', format="X(8)", initial="", max_width=16, label="Address CodC", column_label="CodC", position=26, order=280, help="CodC in address validation")
        t.column('AddressCodP', 'character', format="X(8)", initial="", max_width=16, label="Address CodP", column_label="CodP", position=27, order=290, help="CodP in address validation")
        t.column('DirMarkPost', 'logical', format="yes/no", initial="no", max_width=1, label="Direct Marketing Via Post", column_label="Dir.Mark.Post", position=28, order=200, help="Direct marketing using post")
        t.column('AddressCodM', 'character', format="X(8)", initial="", max_width=16, label="Address CodM", column_label="CodM", position=29, order=310, help="CodM in address validation")
        t.column('BirthDay', 'date', format="99.99.99", max_width=4, label="BirthDay", column_label="BirthDay", position=30, order=350)
        t.index('CustContact', [['Brand'], ['CustNum'], ['CustType']], area="Sta_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('CustContact')
