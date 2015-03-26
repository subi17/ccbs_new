from gearbox.migrations import Migration

class Addusergrouprights(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('usergrouprights', area='Sta_Data_256',
                       dump_name='ugrprigt')
        t.column('usergroup', 'character', format='X(12)', initial='',
                 label='UserGroup')
        t.column('rightgroup', 'character', format='X(12)', initial='',
                 label='RightGroup',
                 help='Right group code')
        t.column('canDisplay', 'logical', initial='no',
                 label='Display',
                 help='Can Display')
        t.column('canCreate', 'logical', initial='no',
                 label='Create',
                 help='Can create')
        t.column('canModified', 'logical', initial='no',
                 label='Modified',
                 help='Can modified')
        t.column('canCopy', 'logical', initial='no',
                 label='Copy',
                 help='Can copy')
        t.column('canDelete', 'logical', initial='no',
                 label='Delete',
                 help='Can Delete')
        t.index('rightgroup', ['rightgroup', 'usergroup'], area='Sta_Index_2',
                unique=True)
        t.index('usergroup', ['usergroup', 'rightgroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('usergrouprights')

