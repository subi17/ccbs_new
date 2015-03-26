from gearbox.migrations import Migration

class AddCLIType(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('CLIType', area='Sta_Data_256',
                       dump_name='mobtype',
                       desc='\
\
\
')
        t.column('Clitype', 'character', initial='',
                 label='CLI Type',
                 column_label='CLIType')
        t.column('CliName', 'character', format='x(25)', initial='',
                 label='CLI Type Name',
                 column_label='Name',
                 help='Name of CLI type')
        t.column('BillTarget', 'integer', format='>9', initial='0',
                 label='Number',
                 column_label='Number',
                 help='Customer\'s billing target')
        t.column('Memo', 'character', extent=15, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('PricePlan', 'character', format='x(12)', initial='',
                 label='PricePlan Code',
                 column_label='PPlan',
                 help='Code of Price Plan')
        t.column('DiscPlan', 'character', format='x(12)', initial='',
                 label='DPCode',
                 column_label='DPlan',
                 help='Code of a Discount Plan')
        t.column('MinimAmt', 'decimal', decimals=2, format='>,>>9.99', initial='0',
                 label='MinimPrice',
                 column_label='MinimPrice',
                 help='Invoice amount (at least)')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Product',
                 column_label='Product',
                 help='Product code, max 16 characters')
        t.column('ServicePack', 'character', initial='',
                 label='ServPack',
                 column_label='ServPack',
                 help='Default ServPack')
        t.column('FeeModel1', 'character', format='x(16)', initial='',
                 label='BEvent',
                 column_label='BEvent',
                 help='An unique code for a first mobsub')
        t.column('FeeModel2', 'character', format='x(16)', initial='',
                 label='BEvent',
                 column_label='BEvent',
                 help='An unique code for a others mobsub')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('PenaltyFee', 'character', initial='',
                 label='Penalty Fee',
                 column_label='Penalty',
                 help='Fee model for penalty fee')
        t.column('ContrType', 'integer', format='9', initial='1',
                 label='Contract Type',
                 column_label='ContrType',
                 help='Default contract type')
        t.column('WatchDogLimit', 'decimal', decimals=2, format='>>,>>9.99', initial='0',
                 label='Wathcdog Limit',
                 column_label='WDLimit',
                 help='Watchdog limit for mobile subsciption')
        t.column('WebDisp', 'logical', initial='no',
                 label='Display in Web',
                 column_label='Web',
                 help='Display CLI type in web')
        t.column('WebInfo', 'character', format='x(60)', initial='',
                 label='Web Info URL',
                 column_label='URL',
                 help='URL for web info')
        t.column('ArAccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Receivables Account',
                 column_label='Receiv.',
                 help='Account no. for Receivables')
        t.column('PerAccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Periodizing Account',
                 column_label='Period.',
                 help='Account for periodizing')
        t.column('UnbillAccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Unbilled Account',
                 column_label='Unbilled',
                 help='Account no. for unbilled events (balance sheet)')
        t.column('ServiceClass', 'character', format='x(4)', initial='',
                 column_label='S.Cl',
                 help='Service class info')
        t.column('PayType', 'integer', format='9', initial='0',
                 label='Payment Type',
                 column_label='PayType',
                 help='Payment type')
        t.index('Clitype', ['Brand', 'Clitype'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CliType_s', ['Clitype'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CLIType')

