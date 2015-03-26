from gearbox.migrations import Migration

class AddMenuTree(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MenuTree', area='Sta_Data_64',
                       label='Menu Tree',
                       dump_name='menutree',
                       desc='Menu configuration, menu tree')
        t.column('Level', 'character', mandatory=True, format='x(16)', initial='',
                 column_label='Level',
                 help='Menu level (main level 0, sublevel f.ex. 134)')
        t.column('Position', 'integer', mandatory=True, format='9', initial='0',
                 label='P',
                 column_label='P',
                 help='Menu slot (1  ...  8), 0 if not a MENU item',
                 valexp='Position < 9',
                 valmsg='Has to be 0 ... 8 !')
        t.column('MenuNum', 'integer', mandatory=True, format='>>9', initial='0',
                 label='Mno',
                 column_label='Mno',
                 help='Menutext\'s number')
        t.column('MenuType', 'integer', mandatory=True, format='9', initial='1',
                 label='T',
                 column_label='T',
                 help='Leads to: 1: program, 2: next menu, 3: previous menu',
                 valexp='MenuType > 0 and MenuType < 4',
                 valmsg='Has to be 1 ... 3 !')
        t.column('Module', 'character', mandatory=True, format='x(10)', initial='',
                 column_label='Module',
                 help='Name of called program or the next menu level')
        t.column('MenuId', 'character', initial='',
                 label='Fcode',
                 column_label='Fcode',
                 help='Function code')
        t.column('MenuTitle', 'character', format='x(40)', initial='',
                 label='Fname',
                 column_label='Fname',
                 help='Function name')
        t.column('HotKey', 'character', initial='',
                 label='Shortcut',
                 column_label='Shortcut',
                 help='Function\'s shortcut')
        t.column('UserRight', 'integer', format='ZZ9', initial='0',
                 label='Privil',
                 column_label='Privil',
                 help='Function privilege')
        t.column('Memo', 'character', extent=32, format='x(75)', initial='',
                 label='Info',
                 column_label='Info',
                 help='Description, information and hints for use')
        t.column('MenuClass', 'integer', format='zzz9', initial='0',
                 label='PgCl',
                 column_label='PgCl',
                 help='Unique number for Program Class')
        t.column('State', 'logical', extent=3, initial='no',
                 label='Status',
                 column_label='Status',
                 help='Status code')
        t.column('TokenCode', 'character', format='X(12)', initial='',
                 label='Token',
                 help='Comma separed list of Token codes')
        t.index('HotKey', ['HotKey'], area='Sta_Index_2')
        t.index('Level', ['Level', 'Position'], area='Sta_Index_2',
                primary=True)
        t.index('MenuClass', ['MenuClass', 'MenuId'], area='Sta_Index_2')
        t.index('MenuId', ['MenuId'], area='Sta_Index_2')
        t.index('MenuTitle', ['MenuTitle', 'MenuId'], area='Sta_Index_2')
        t.index('Module', ['Module', 'Level', 'Position'], area='Sta_Index_2')

    def down(self):
        self.drop_table('MenuTree')

