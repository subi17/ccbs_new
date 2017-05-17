from gearbox.migrations import Migration

class AddTableTMSParam(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TMSParam', area="Sta_Data_128", label="Configuration Parameters", dump_name="tmsparam", desc="Company parameters")
        t.column('ParamGroup', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Group", column_label="Group", position=2, order=10, help="Parameter Group")
        t.column('ParamCode', 'character', format="x(24)", initial="", max_width=48, label="Code", column_label="Code", position=3, order=20, help="Parameter code (description)")
        t.column('IntVal', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="Integer", column_label="Integer", position=4, order=40, help="Integer value")
        t.column('DecVal', 'decimal', format="->>>>>>>9.99<<<<", decimals=6, initial="0", max_width=21, label="Decimal", column_label="Decimal", position=5, order=50, help="Decimal value")
        t.column('CharVal', 'character', format="x(50)", initial="", max_width=100, label="Char", column_label="Char", position=6, order=60, help="Character value")
        t.column('DateVal', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=7, order=70, help="Date")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=8, order=80, help="Memo")
        t.column('ParamType', 'character', valexp="lookup(ParamType,\"I,C,DE,DA\") > 0", format="xx", initial="", max_width=4, label="Type", column_label="Type", position=9, order=30, valmsg="Valid Type Codes are (I)nt (C)har (DE)cimal (DA)te", help="Type of character (I,C,DE,DA)")
        t.column('ParamName', 'character', format="x(30)", initial="", max_width=60, label="Parameter", column_label="Parameter", position=10, order=90, help="Name of parameter")
        t.column('Online', 'logical', format="Y/N", initial="no", max_width=1, label="OnLine", column_label="OnLine", position=11, order=100, help="Is this parameter used with OnLine connection ?")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=12, order=110, help="Code Of Brand")
        t.index('ParamGroup', [['Brand'], ['ParamGroup'], ['ParamCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ParamCode', [['Brand'], ['ParamCode'], ['ParamGroup']], area="Sta_Index_2", unique=True)
        t.index('ParamName', [['Brand'], ['ParamName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TMSParam')
