from gearbox.migrations import Migration

class AddDiscGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DiscGroup', area='Sta_Data_256',
                       label='Discount Groups',
                       dump_name='discgrp',
                       desc='Groups for different destinations in order to calculate\
volume discounts; used in Discount Plans\
')
        t.column('DiscGroup', 'character', initial='',
                 label='Discount Group',
                 column_label='Discount Group',
                 help='Code of Discount Group')
        t.column('DGName', 'character', format='x(30)', initial='',
                 label='Disc Grp Name',
                 column_label='Discount Group Name',
                 help='Name of Discount Group')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo of DiscountGroup')
        t.index('DGName', ['DGName', 'DiscGroup'], area='Sta_Index_2',
                unique=True)
        t.index('DiscGroup', ['DiscGroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('DiscGroup')

