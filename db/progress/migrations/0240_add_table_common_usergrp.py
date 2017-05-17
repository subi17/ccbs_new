from gearbox.migrations import Migration

class AddTableUserGrp(Migration):

    database = "common"

    def up(self):
        t = self.table('UserGrp', area="Sta_Data_64", label="User Groups", dump_name="usergrp", desc="User Groups")
        t.column('UserGroup', 'character', format="x(10)", initial="", max_width=20, label="GroupCode", column_label="GroupCode", position=2, order=10, help="Individual Code for a User Group")
        t.column('UGName', 'character', format="x(40)", initial="", max_width=80, label="Name", column_label="Name", position=3, order=20, help="Name of user group")
        t.column('CreDate', 'date', format="99-99-99", initial="today", max_width=4, label="Created", column_label="Created", position=4, order=30, help="Date When Group Was Created")
        t.column('ChgDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Changed", column_label="Changed", position=5, order=40, help="Date when Group and/or its members were changed")
        t.column('CreUser', 'character', format="x(8)", initial="", max_width=16, label="Created by", column_label="Created by", position=6, order=50, help="User who created this group")
        t.column('ChgUser', 'character', format="x(8)", initial="", max_width=16, label="Changed by", column_label="Changed by", position=7, order=400, help="User who changed/updated this group latest")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1830, label="Memo", column_label="Memo", extent=15, position=8, order=410, help="Memo text")
        t.column('ShowTokens', 'character', format="x(50)", initial="", max_width=100, label="Show Tokens", position=9, order=420, help="Comma separed list of Token codes")
        t.column('ModifyTokens', 'character', format="x(50)", initial="", max_width=100, label="Modify Tokens", position=10, order=430, help="Comma separed list of Token codes")
        t.column('PasswordExpires', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Password expires", position=11, order=440, help="Shall the passwords of this group expire")
        t.index('UserGroup', [['UserGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('UGName', [['UGName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('UserGrp')
