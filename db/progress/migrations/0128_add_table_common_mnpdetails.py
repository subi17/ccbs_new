from gearbox.migrations import Migration

class AddMNPDetails(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPDetails', area='Sta_Data_64',
                       dump_name='mnpdetails')
        t.column('MNPSeq', 'integer', format='>>>>>>>9', initial='0')
        t.column('CustId', 'character', format='x(12)', initial='',
                 column_label='CustId')
        t.column('CustIdType', 'character', initial='',
                 column_label='CustIdType')
        t.column('FirstName', 'character', format='x(20)', initial='',
                 label='ForeName',
                 column_label='ForeName',
                 help='Customer\'s fore/given name',
                 description='Customer\'s forename')
        t.column('SurName1', 'character', format='x(20)', initial='',
                 column_label='SurName1',
                 help='Customer\'s 1st last name',
                 description='Customer\'s 1st last name')
        t.column('SurName2', 'character', format='x(20)', initial='',
                 column_label='SurName2',
                 help='Customer\'s 1st last name',
                 description='Customer\'s 1st last name')
        t.column('Nationality', 'character', format='x(2)', initial='',
                 column_label='Nationality')
        t.column('ReceptorCode', 'character', format='x(3)', initial='',
                 column_label='ReceptorCode')
        t.column('CompanyName', 'character', format='x(30)', initial='',
                 column_label='CompanyName',
                 help='Company name')
        t.column('ReceptorNRN', 'character', initial='',
                 column_label='ReceptorNRN',
                 help='Receptor operator NRN')
        t.column('PortingTimeFromCustomer', 'logical', initial='No',
                 column_label='PortingTimeFromCustomer',
                 help='Is the porting time defined by customer')
        t.column('DonorCode', 'character', format='x(3)', initial='',
                 column_label='DonorCode',
                 help='Donor operator Code')
        t.column('RequestedTS', 'decimal', format='99999999.99999', initial='0',
                 column_label='RequestedTS',
                 help='Process creation time on operator side')
        t.column('StatusLimitTS', 'decimal', format='99999999.99999', initial='0',
                 column_label='StatusLimitTS',
                 help='Time limit for status change')
        t.column('DonorExtraOrdinary', 'logical', initial='No',
                 column_label='DonorExtraOrdinary',
                 help='Donor operator is in extraordinary state')
        t.index('CustId', ['CustId'], area='Sta_Index_2')
        t.index('MNPSeq', ['MNPSeq'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('MNPDetails')

