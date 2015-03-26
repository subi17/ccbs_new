from gearbox.migrations import Migration

class AddDumpFile(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DumpFile', area='Sta_Data_128',
                       label='Dump File',
                       dump_name='dumpfile',
                       desc='Dump file')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('Description', 'character', format='x(50)', initial='',
                 help='Description of dump')
        t.column('DumpID', 'integer', format='>>>>>>>9', initial='0',
                 label='Dump ID',
                 column_label='ID',
                 help='Unique ID')
        t.column('FileName', 'character', format='x(50)', initial='',
                 label='File Name',
                 column_label='File',
                 help='File name')
        t.column('SpoolDir', 'character', format='x(50)', initial='',
                 label='Spool Directory',
                 column_label='Spool',
                 help='Spool directory')
        t.column('TransDir', 'character', format='x(50)', initial='',
                 label='Transfer Directory',
                 column_label='Transfer',
                 help='Transfer directory')
        t.column('Active', 'logical', initial='yes',
                 help='Is dump active')
        t.column('DumpDelimiter', 'character', initial='',
                 label='Delimiter')
        t.column('DecimalPoint', 'character', initial='',
                 label='Decimal Point',
                 column_label='Dec.Point',
                 help='Decimal point (numeric-format)')
        t.column('MainTable', 'character', format='x(30)', initial='',
                 label='Main Table',
                 column_label='Table',
                 help='Main table')
        t.column('SideTables', 'character', format='x(50)', initial='',
                 label='Additional Tables',
                 column_label='Side Tables',
                 help='Additional tables')
        t.column('LinkKey', 'character', format='x(20)', initial='',
                 label='Link Key',
                 column_label='Key',
                 help='Key field that is used to link the side tables to main table')
        t.column('DumpLineFeed', 'character', initial='',
                 label='Line Feed',
                 help='Line feed between rows')
        t.column('DumpCharSet', 'character', format='x(20)', initial='',
                 label='Character Set',
                 column_label='Char.Set',
                 help='Character set for the file')
        t.column('DumpName', 'character', format='x(20)', initial='',
                 label='Dump Name',
                 column_label='Name',
                 help='Unique name for the dump')
        t.column('DumpFormat', 'character', initial='',
                 label='Dump Format',
                 column_label='Format',
                 help='Dump file format')
        t.column('EmptyFile', 'logical', initial='no',
                 label='Create Empty File',
                 column_label='Empty File',
                 help='Create always a file even if events don\'t exist')
        t.column('FileCategory', 'character', format='x(12)', initial='',
                 label='File Category',
                 column_label='Category',
                 help='File category')
        t.column('LogFile', 'character', format='x(40)', initial='',
                 label='Log File',
                 column_label='Log',
                 help='Log file')
        t.column('QueryClause', 'character', format='x(30)', initial='',
                 label='Query Clause',
                 column_label='Query',
                 help='Query clause used to retrieve records for the dump')
        t.column('UseIndex', 'character', format='x(20)', initial='',
                 label='Use Index',
                 column_label='Index',
                 help='Index used for retrieving records')
        t.column('LogicModule', 'character', format='x(30)', initial='',
                 label='Logic Module',
                 column_label='Module',
                 help='Module that contains logic for data collection')
        t.column('ModFromEventLog', 'logical', initial='no',
                 label='Check EventLog',
                 column_label='EventLog',
                 help='Check modifications from EventLog')
        t.column('ModFromField', 'character', format='x(30)', initial='',
                 label='Check Field',
                 column_label='Field',
                 help='Check modification from (timestamp) field')
        t.column('ModCollModule', 'character', format='x(30)', initial='',
                 label='Collect Modified',
                 column_label='Collection',
                 help='Module for collecting modified events')
        t.column('FullCollModule', 'character', format='x(30)', initial='',
                 label='Collect All',
                 column_label='Full Coll.',
                 help='Module for collecting all events for a full dump')
        t.column('ConfigParam', 'character', format='x(20)', initial='',
                 label='Configuration Parameters',
                 column_label='Parameter',
                 help='Configuration parameters for the external procedures')
        t.column('EventLogFields', 'character', format='x(30)', initial='',
                 label='EventLog Fields',
                 column_label='Mod.Fields',
                 help='Modified fields that will be checked from EventLog')
        t.index('Description', ['Brand', 'Description'], area='Sta_Index_2')
        t.index('DumpID', ['DumpID'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('DumpName', ['Brand', 'DumpName'], area='Sta_Index_2')
        t.index('FileName', ['Brand', 'FileName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('DumpFile')

