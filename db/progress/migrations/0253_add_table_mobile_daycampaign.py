from gearbox.migrations import Migration

class AddDayCampaign(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('DayCampaign', area='Sta_Data_256',
                       dump_name='daycampa')
        t.column('DCEvent', 'character', format='x(12)', initial='',
                 label='Periodical Term',
                 column_label='ID',
                 help='ID of periodical term')
        t.column('ValidFrom', 'date', format='99-99-9999',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('InclUnit', 'integer', format='>9', initial='0',
                 label='Included Unit',
                 column_label='Incl.Unit',
                 help='Unit of included material')
        t.column('ValidTo', 'date', format='99-99-9999',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillCode',
                 help='Billing item code')
        t.column('MaxChargeIncl', 'decimal', decimals=3, format='->>,>>9.999', initial='0',
                 label='Max. Charge Incl. VAT',
                 column_label='MaxIncl',
                 help='Maximum charge including VAT')
        t.column('DCTarget', 'character', format='x(12)', initial='',
                 label='Target',
                 column_label='Target',
                 help='Target (allowed billing item)')
        t.column('Weekday', 'character', format='x(7)', initial='',
                 column_label='Weekday',
                 help='Weekday (1=Sunday, 2=monday...7=Saturday)')
        t.column('DCName', 'character', format='x(60)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of periodical term')
        t.column('CCN', 'integer', format='>>>9', initial='0',
                 help='Report CCN that is marked to CDRs')
        t.column('MaxChargeExcl', 'decimal', decimals=3, format='->>>>>>>9.999', initial='0',
                 label='Max. Charge Excl.VAT',
                 column_label='MaxExcl',
                 help='Maximum charge excluding VAT')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('DCType', 'character', initial='',
                 label='Campaign Type',
                 column_label='Type',
                 help='Campaign type')
        t.column('CalcMethod', 'integer', format='9', initial='0',
                 label='Calculation Method',
                 column_label='Method',
                 help='Calculation method')
        t.column('DurType', 'integer', format='9', initial='0',
                 label='Type Of Duration',
                 column_label='Dur.Type',
                 help='Type of duration')
        t.column('DurMonths', 'integer', format='>>9', initial='0',
                 label='Duration In Months',
                 column_label='Duration',
                 help='Duration in months')
        t.column('Renewal', 'integer', format='9', initial='0',
                 label='Renewal Method',
                 column_label='Renewal',
                 help='Renewal method')
        t.column('FeeModel', 'character', format='x(12)', initial='',
                 label='Creation Fee Model',
                 column_label='Creation FM',
                 help='Fee model for creating contract')
        t.column('ModifyFeeModel', 'character', format='x(12)', initial='',
                 label='Modification Fee Model',
                 column_label='Mod.FM',
                 help='Fee model for modifying contract')
        t.column('TermFeeModel', 'character', format='x(12)', initial='',
                 label='Termination Fee Model',
                 column_label='Term.FM',
                 help='Fee model for terminating contract')
        t.column('DurUnit', 'integer', format='>9', initial='0',
                 label='Duration Unit',
                 column_label='Dur.Unit',
                 help='Duration unit')
        t.column('Effective', 'integer', format='>9', initial='0',
                 column_label='Eff.',
                 help='When contract becomes effective')
        t.column('TermFeeCalc', 'integer', format='>9', initial='0',
                 label='Term. Fee Calculation',
                 column_label='TFee Calc',
                 help='Termination fee calculation method')
        t.index('DCEvent', ['Brand', 'DCEvent'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('DayCampaign')

