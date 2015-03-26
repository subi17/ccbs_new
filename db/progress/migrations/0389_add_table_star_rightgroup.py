from gearbox.migrations import Migration

class Addrightgroup(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('rightgroup', area='Sta_Data_256',
                       label='Right groups',
                       dump_name='rightgrp')
        t.column('rightgroup', 'character', format='X(12)', initial='',
                 label='RightGroup',
                 help='Right group code')
        t.column('name', 'character', format='X(40)', initial='',
                 label='Name',
                 help='Rightgroup name')
        t.index('rightgroup', ['rightgroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('rightgroup')

