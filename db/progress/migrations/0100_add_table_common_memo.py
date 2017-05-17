from gearbox.migrations import Migration

class AddTableMemo(Migration):

    database = "common"

    def up(self):
        t = self.table('Memo', area="Sta_Data_32", label="Memo pool", dump_name="memo", desc='''This file is a common storage area for all kind of memos
assigned to different tables/records''')
        t.column('HostTable', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="HostTable", column_label="HostTable", position=2, order=10, help="Name of table where this memo record is assigned")
        t.column('KeyValue', 'character', mandatory=True, format="x(16)", initial="", help="Unique value of hosting record", max_width=32, label="KeyValue", column_label="KeyValue", position=3, order=20, description="Key value of hosting rec converted into formatted string expression")
        t.column('MemoSeq', 'integer', mandatory=True, format=">>>>>>>>>>9", initial="0", help="Unique, consecutive sequence no. of memo record", max_width=4, label="Seq", column_label="Seq", position=4, order=30, description="sequence from memoSeq sequence")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time when this memo record was CREATED", max_width=20, label="Created", column_label="Created", position=5, order=40, description="Time Stamp yyyymmdd.time (sec)")
        t.column('CreUser', 'character', format="X(8)", initial="", max_width=16, label="Created By", column_label="Created by", position=6, order=50, help="User who CREATED this memo record")
        t.column('ChgStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Modification Date and Time", max_width=20, label="ModTime", column_label="ModTime", position=7, order=60, description="Time Stamp yyyymmdd.time (sec)")
        t.column('ChgUser', 'character', format="x(8)", initial="", max_width=16, label="Mod By", column_label="Mod By", position=8, order=70, help="User who MODIFIED this memo")
        t.column('MemoTitle', 'character', format="x(40)", initial="", max_width=80, label="Title", column_label="Title/topic", position=9, order=80, help="Title or topic of this memo")
        t.column('MemoText', 'character', format="x(900)", initial="", max_width=1800, label="MemoText", column_label="MemoText", position=10, order=90, help="MemoText")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=11, order=100, help="Code Of Brand")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=12, order=110, help="Customer's number")
        t.column('MemoType', 'character', format="x(12)", initial="", max_width=24, label="MemoType", column_label="MemoType", position=13, order=120, help="Type of memo")
        t.column('Priority', 'integer', format=">>9", initial="0", max_width=4, label="Priority", column_label="Pri", position=14, order=130, help="Priority")
        t.column('Source', 'character', format="x(8)", initial="", max_width=16, label="Source", column_label="Source", position=15, order=140)
        t.index('HostTable', [['Brand'], ['HostTable'], ['KeyValue'], ['MemoSeq', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['HostTable'], ['KeyValue']], area="Sta_Index_2")
        t.index('CustOrder', [['Brand'], ['CustNum'], ['MemoSeq', 'DESC']], area="Sta_Index_2")
        t.index('MemoSeq', [['MemoSeq']], area="Sta_Index_2", unique=True)
        t.index('MemoType', [['Brand'], ['MemoType'], ['CustNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Memo')
