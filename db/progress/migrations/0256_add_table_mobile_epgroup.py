from gearbox.migrations import Migration

class AddEPGroup(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('EPGroup', area='Sta_Data_256',
                       label='Product group',
                       dump_name='epgrp',
                       desc='External product group')
        t.column('EpGroup', 'character', format='x(12)', initial='',
                 label='EPcode',
                 column_label='EPcode',
                 help='Unique code of product group')
        t.column('EpName', 'character', format='x(40)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of product group')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('EpGroup', ['Brand', 'EpGroup'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('EpName', ['Brand', 'EpName', 'EpGroup'], area='Sta_Index_3',
                unique=True)

    def down(self):
        self.drop_table('EPGroup')

