from gearbox.migrations import Migration

class AddVRKQuery(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('VRKQuery', area='Sta_Data_32',
                       label='VRKQuery',
                       dump_name='vrkquery')
        t.column('PersonId', 'character', format='x(11)', initial='',
                 column_label='PersonId',
                 help='Personal Number',
                 description='Personal Number')
        t.column('CrStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Creation timestamp',
                 description='Creation timestamp')
        t.column('LastName', 'character', format='x(20)', initial='',
                 column_label='LastName',
                 help='Customer\'s last name',
                 description='Customer\'s last name')
        t.column('FirstName', 'character', format='x(20)', initial='',
                 label='ForeName',
                 column_label='ForeName',
                 help='Customer\'s fore/given name',
                 description='Customer\'s forename')
        t.column('HomeCode', 'character', format='x(3)', initial='',
                 column_label='HomeCode')
        t.column('HomeName', 'character', format='x(20)', initial='',
                 column_label='HomeName')
        t.column('Address', 'character', format='x(30)', initial='',
                 column_label='Address',
                 help='Customer\'s mailing address (street, p.o. box)')
        t.column('ZipCode', 'character', initial='',
                 label='Postal code',
                 column_label='Postcd',
                 help='Customer\'s postal code')
        t.column('PostOffice', 'character', format='x(24)', initial='',
                 label='Postal Addr.',
                 column_label='Postaddr',
                 help='Customer\'s postal address')
        t.column('MovingDate', 'date',
                 column_label='MovingDate')
        t.column('TempAddress', 'character', format='x(20)', initial='',
                 column_label='TempAddress',
                 description='Customer\'s temporary address')
        t.column('TempZip', 'character', format='x(5)', initial='',
                 column_label='TempZip',
                 description='Customer\'s temporary zipcode')
        t.column('TempPOffice', 'character', format='x(24)', initial='',
                 column_label='TempPOffice',
                 description='Customer\'s temporary post office')
        t.column('TempFrom', 'date',
                 column_label='TempFrom',
                 description='Temporary address valid from')
        t.column('TempTo', 'date',
                 column_label='TempTo',
                 description='Temporary address valid to')
        t.column('Language', 'character', format='x(2)', initial='',
                 column_label='Language',
                 description='Language code')
        t.column('LangName', 'character', format='x(30)', initial='',
                 column_label='LangName',
                 description='Language in text')
        t.column('DeathDay', 'date',
                 column_label='DeathDay',
                 description='Date of death')
        t.column('Trusteeship', 'character', format='x(1)', initial='',
                 column_label='Tship',
                 description='Edunvalvonta')
        t.column('CompLimit', 'character', format='x(1)', initial='',
                 label='Competence Limit',
                 column_label='CompLimit',
                 help='Limitation of competence',
                 description='toimintakelpoisuuden rajoitus')
        t.column('TshipTxt', 'character', format='x(20)', initial='',
                 label='Trusteeship Text',
                 column_label='Tship Txt',
                 help='Trusteeship as text',
                 description='Edunvalvonta tekstina')
        t.column('CompLimitTxt', 'character', format='x(20)', initial='',
                 label='Comp.Limit Text',
                 column_label='CLimit Txt',
                 help='Limitation of competence as text',
                 description='rajoitus tekstina')
        t.index('PersonId', ['PersonId', ('CrStamp', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('VRKQuery')

