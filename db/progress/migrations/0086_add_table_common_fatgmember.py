from gearbox.migrations import Migration

class AddFATGMember(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FATGMember', area='Sta_Data_128',
                       label='FAT Group Members',
                       dump_name='fatgmemb',
                       desc='Members for the FAT Group (FtGrp)')
        t.column('FtGrp', 'character', initial='',
                 label='FatGroup',
                 column_label='FtGrp',
                 help='Fat Group (for products)')
        t.column('MemberType', 'integer', format='9', initial='0',
                 label='Member Type',
                 column_label='Type',
                 help='Type of the member (product, FAT Group)')
        t.column('FtgMember', 'character', format='x(16)', initial='',
                 label='Group Member',
                 column_label='Member',
                 help='Group member')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Explanation / memory field for FTGmember')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('FtGrp', ['Brand', 'FtGrp', 'MemberType', 'FtgMember'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('FATGMember')

