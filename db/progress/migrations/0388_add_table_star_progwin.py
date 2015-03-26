from gearbox.migrations import Migration

class Addprogwin(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('progwin', area='Sta_Data_256',
                       label='Program Windows')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('windownumber', 'integer', format='>9', initial='0',
                 label='Window',
                 help='Window number. Number 0 is automicly started')
        t.column('parentwindow', 'integer', format='>>9', initial='0',
                 label='ParentWindow',
                 help='Parent Window number. Foreign keys should found there')
        t.column('objectType', 'character', initial='Smart',
                 label='Type',
                 help='Object type')
        t.column('object', 'character', format='X(50)', initial='',
                 label='Object',
                 help='Object filename. Blank for browsers')
        t.column('appsrv', 'character', mandatory=True, format='X(20)', initial='',
                 label='AppServer',
                 help='Logical name of Application Server')
        t.column('sdo', 'character', format='X(50)', initial='',
                 label='SDO',
                 help='Program with recursize directory')
        t.column('displayfields', 'character', format='X(50)', initial='',
                 label='Displayed',
                 help='Displayed fields. Blank for static objects')
        t.column('enabledFields', 'character', format='X(50)', initial='',
                 label='Enabled',
                 help='Enabled fields. Blank for static objects')
        t.column('foreingFields', 'character', format='X(50)', initial='',
                 label='ForeignFields',
                 help='Foreign fields if needed')
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
        t.column('title-command', 'character', format='X(50)', initial='',
                 label='Title',
                 help='Title command... $as-nro $as-nimi')
        t.column('treeCode', 'character', format='X(12)', initial='',
                 label='TreeCode',
                 help='Exsample 1.1.2')
        t.column('TreeType', 'character', initial='Program',
                 help='Tree type')
        t.column('name', 'character', format='X(40)', initial='',
                 label='Menu name')
        t.column('rightgroup', 'character', format='X(12)', initial='',
                 label='RightGroup',
                 help='Right group code')
        t.column('helptext', 'character', format='X(256)', initial='',
                 label='Helptext',
                 help='Program helptext.')
        t.index('treecode', ['programcode', 'treeCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('windownumber', ['programcode', 'windownumber'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('progwin')

