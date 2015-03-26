from gearbox.migrations import Migration

class AddCustPP(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustPP', area='Sta_Data_256',
                       label='Customer\'s Prod Packages',
                       dump_name='custpp',
                       desc='Customer\'s Prod Packages')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('ProdPack', 'character', mandatory=True, initial='',
                 label='PpId',
                 column_label='PpId',
                 help='Product Package ID')
        t.column('CustPP', 'integer', format='->>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Internal sequence no of Contract')
        t.column('ValidFrom', 'date', mandatory=True, format='99-99-99',
                 label='CDate',
                 column_label='ContDate',
                 help='Date from which the contract is valid')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='ExpDate',
                 column_label='ExpDate',
                 help='Date when contract was ended/expired')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('Reseller', 'character', initial='',
                 column_label='Reseller',
                 help='Code of reseller (if applicable)')
        t.column('xxMemo', 'character', extent=5, format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo',
                 help='Explanation / memory field for Customer\'s ProdPack')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CustNum', ['Brand', 'CustNum', 'ProdPack', 'CustPP'], area='Sta_Index_2',
                unique=True)
        t.index('CustNum_s', ['CustNum', 'ProdPack', 'CustPP'], area='Sta_Index_2',
                unique=True)
        t.index('CustPP', ['Brand', 'CustPP'], area='Sta_Index_2',
                unique=True)
        t.index('ProdPack', ['Brand', 'ProdPack', 'CustNum'], area='Sta_Index_2',
                primary=True)
        t.index('Reseller', ['Brand', 'Reseller'], area='Sta_Index_2')
        t.index('Salesman', ['Brand', 'Salesman'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CustPP')

