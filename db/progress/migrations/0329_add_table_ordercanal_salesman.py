from gearbox.migrations import Migration

class AddSalesman(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('Salesman', area='Sta_Data_256',
                       label='Salesman',
                       dump_name='salesman',
                       desc='Salesman')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('SmName', 'character', format='x(30)', initial='',
                 label='Salesman Name',
                 column_label='Salesman Name',
                 help='Salesman\'s name')
        t.column('SalesOffice', 'character', mandatory=True, initial='',
                 column_label='SalesOffice',
                 help='Sales office code')
        t.column('EMail', 'character', format='x(40)', initial='',
                 label='E-mail',
                 column_label='E-mail',
                 help='Salesman\'s e-mail address')
        t.column('Parent', 'character', initial='',
                 label='Parent Salesman',
                 column_label='Parent Salesman',
                 help='Salesman\'s parent code')
        t.column('CommPerc', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='Prov%',
                 column_label='Prov%',
                 help='Commission %')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Reseller', 'character', initial='',
                 column_label='Resell',
                 help='Reseller code')
        t.column('RsLevel', 'integer', format='>9', initial='0',
                 label='Reseller Level',
                 column_label='RSLevel',
                 help='Salesman\'s level in reseller\'s organization')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer that is related to this salesman')
        t.column('Active', 'logical', initial='yes',
                 help='Is salesman active')
        t.index('CustNum', ['Brand', 'CustNum', 'Salesman'], area='Sta_Index_2')
        t.index('Reseller', ['Brand', 'Reseller', 'RsLevel', 'Salesman'], area='Sta_Index_2')
        t.index('Salesman', ['Brand', 'Salesman', 'SmName'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('SalesOffice', ['Brand', 'SalesOffice', 'Salesman'], area='Sta_Index_2',
                unique=True)
        t.index('SmName', ['Brand', 'SmName', 'SalesOffice'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Salesman')

