from gearbox.migrations import Migration

class AddServCom(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('ServCom', area='Sta_Data_64',
                       label='Servcom',
                       dump_name='servcom',
                       desc='Service Component')
        t.column('Service', 'character', initial='',
                 label='Service Group',
                 column_label='Service',
                 help='Code of service group')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Service Component',
                 column_label='Component',
                 help='Code of Service Component')
        t.column('ScName', 'character', format='x(60)', initial='',
                 label='Component Name',
                 column_label='Name',
                 help='Name of Service Component')
        t.column('memo', 'character', extent=10, format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo',
                 help='Memo of Service Component')
        t.column('ScLocalName', 'character', format='x(60)', initial='',
                 label='Local Name',
                 column_label='Local Name',
                 help='Name in Local Language')
        t.column('ServiceLimit', 'character', format='x(16)', initial='',
                 label='Service Limit',
                 column_label='SLimit',
                 help='Service limit group')
        t.column('ScParameter', 'logical', initial='no',
                 label='Parameter?',
                 column_label='Parameter?',
                 help='Does this service contain a subscriber-specific parameter ?')
        t.column('ScValueRange', 'integer', extent=2, format='>>>9', initial='0',
                 label='Range',
                 column_label='Range',
                 help='Value Range for a Service Parameter in HLR')
        t.column('ScChgable', 'logical', initial='Yes',
                 label='Changeable',
                 column_label='ChgAble',
                 help='Changeable on subscription level')
        t.column('ScPosition', 'integer', format='zz9', initial='0',
                 label='Pos',
                 column_label='Pos',
                 help='Position')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('FeeModel', 'character', format='x(12)', initial='',
                 label='Opening Fee Model',
                 column_label='Open FM',
                 help='Fee model for opening service')
        t.column('ActType', 'integer', format='9', initial='0',
                 label='Activation Type',
                 column_label='Activ.',
                 help='Activation method type')
        t.column('Target', 'integer', format='9', initial='0',
                 column_label='Target',
                 help='This Service belongs to')
        t.column('ServAttrL', 'logical', initial='no',
                 label='Service Attributes',
                 column_label='Attributes',
                 help='Does component contain attributes')
        t.column('ServType', 'integer', format='9', initial='0',
                 label='Service Type',
                 column_label='ServType',
                 help='Service type; basic / additional')
        t.column('SepHLR', 'logical', initial='no',
                 label='Separate Command',
                 column_label='SepHLR',
                 help='Separate command line to HLR')
        t.column('ClFeeModel', 'character', format='x(12)', initial='',
                 label='Closing Fee Model',
                 column_label='Close FM',
                 help='Fee model for closing service')
        t.column('ParFeeModel', 'character', format='x(12)', initial='',
                 label='Parameter Fee Model',
                 column_label='Param FM',
                 help='Fee model for changing parameter')
        t.column('ChgFeeModel', 'character', format='x(12)', initial='',
                 label='Changing Fee Model',
                 column_label='ChangeFM',
                 help='Fee model for changing service\'s value')
        t.column('CloseTime', 'integer', format='>>>>9', initial='0',
                 label='Closing Time',
                 column_label='Close',
                 help='Closing time rule')
        t.column('SMSTxt', 'character', format='x(12)', initial='',
                 label='SMS Text',
                 column_label='SMS',
                 help='Key value of the information text for SMS')
        t.column('NWElement', 'character', initial='',
                 label='Network Element',
                 column_label='NWE',
                 help='Network element')
        t.column('ClSMSTxt', 'character', format='x(12)', initial='',
                 label='SMS Text For Closing',
                 column_label='Close SMS',
                 help='Key value of the information text for SMS for closing')
        t.column('ChgSMSTxt', 'character', format='x(12)', initial='',
                 label='SMS Text For Changing',
                 column_label='Change SMS',
                 help='Key value of the information text for SMS for changing value')
        t.index('SCName', ['Brand', 'ScName'], area='Sta_Index_3')
        t.index('ServCom', ['Brand', 'ServCom'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('Service', ['Brand', 'Service', 'ServCom'], area='Sta_Index_3')

    def down(self):
        self.drop_table('ServCom')

