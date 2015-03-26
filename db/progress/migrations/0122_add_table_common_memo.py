from gearbox.migrations import Migration

class AddMemo(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Memo', area='Sta_Data_32',
                       label='Memo pool',
                       dump_name='memo',
                       desc='This file is a common storage area for all kind of memos\
assigned to different tables/records')
        t.column('HostTable', 'character', mandatory=True, format='x(12)', initial='',
                 column_label='HostTable',
                 help='Name of table where this memo record is assigned')
        t.column('KeyValue', 'character', mandatory=True, format='x(16)', initial='',
                 column_label='KeyValue',
                 help='Unique value of hosting record',
                 description='Key value of hosting rec converted into formatted string expression')
        t.column('MemoSeq', 'integer', mandatory=True, format='>>>>>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Unique, consecutive sequence no. of memo record',
                 description='sequence from memoSeq sequence')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and time when this memo record was CREATED',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('CreUser', 'character', initial='',
                 label='Created By',
                 column_label='Created by',
                 help='User who CREATED this memo record')
        t.column('ChgStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ModTime',
                 column_label='ModTime',
                 help='Modification Date and Time',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('ChgUser', 'character', initial='',
                 label='Mod By',
                 column_label='Mod By',
                 help='User who MODIFIED this memo')
        t.column('MemoTitle', 'character', format='x(40)', initial='',
                 label='Title',
                 column_label='Title/topic',
                 help='Title or topic of this memo')
        t.column('MemoText', 'character', format='x(900)', initial='',
                 column_label='MemoText')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('MemoType', 'character', format='x(12)', initial='',
                 column_label='MemoType',
                 help='Type of memo')
        t.column('Priority', 'integer', format='>>9', initial='0',
                 column_label='Pri')
        t.index('CustNum', ['Brand', 'CustNum', 'HostTable', 'KeyValue'], area='Sta_Index_2')
        t.index('CustOrder', ['Brand', 'CustNum', ('MemoSeq', 'DESCENDING')], area='Sta_Index_2')
        t.index('HostTable', ['Brand', 'HostTable', 'KeyValue', ('MemoSeq', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('MemoSeq', ['MemoSeq'], area='Sta_Index_2',
                unique=True)
        t.index('MemoType', ['Brand', 'MemoType', 'CustNum'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Memo')

