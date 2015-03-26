from gearbox.migrations import Migration

class AddDCCli(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('DCCli', area='Sta_Data_128',
                       dump_name='dccli')
        t.column('MSSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='Sub.ID',
                 help='Sequence for a subscription')
        t.column('ValidFrom', 'date', format='99-99-9999',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ValidTo', 'date', format='99-99-9999',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('DCEvent', 'character', format='x(12)', initial='',
                 label='Periodical Term',
                 column_label='Term',
                 help='ID of periodical term')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='MSISDN',
                 column_label='CLI')
        t.column('ContractDate', 'date', format='99-99-99',
                 label='Contract Date',
                 column_label='Contract',
                 help='Date when contract was originally signed')
        t.column('TermDate', 'date', format='99-99-99',
                 label='Termination Date',
                 column_label='Terminate',
                 help='Date when contract when will be terminated')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CreateFees', 'logical', initial='yes',
                 label='Create Fees',
                 column_label='Fees',
                 help='Create fees when contract is changed or terminated')
        t.column('RenewalDate', 'date', format='99-99-99',
                 label='Renewal Date',
                 column_label='Renewal',
                 help='Date when contract was renewed')
        t.column('ValidToOrig', 'date', format='99-99-9999',
                 label='Valid To Original',
                 column_label='ValidToOrig',
                 help='Original Valid To')
        t.column('PerContractID', 'integer', format='>>>>>>>9', initial='0',
                 label='Periodical Contract ID',
                 column_label='Per.Contr.',
                 help='Periodical contract ID',
                 description='unique sequence ID')
        t.index('Contract', ['MSSeq', 'DCEvent'], area='Sta_Index_2',
                primary=True)
        t.index('DCEvent', ['Brand', 'DCEvent', 'MSSeq', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('MSSeq', ['MSSeq', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('DCCli')

