from gearbox.migrations import Migration

class Addrsoper(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('rsoper', area='Sta_Data_256',
                       label='Reseller Operators',
                       desc='Reseller operators')
        t.column('CustNum', 'integer', mandatory=True, format='zzzzzz9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer\'s number')
        t.column('Operator', 'character', mandatory=True, initial='',
                 label='OpCode',
                 column_label='OpCode',
                 help='Operator\'s code, 1 - 8 characters')
        t.column('fileid', 'character', format='x(4)', initial='',
                 label='FileID',
                 column_label='FileID',
                 help='File identification prefix')
        t.column('req-script', 'character', format='x(20)', initial='',
                 label='REQ Script',
                 column_label='REQ Script',
                 help='Name of the request script')
        t.column('cdr-script', 'character', format='x(20)', initial='',
                 label='CDR Script',
                 column_label='CDR Script',
                 help='Name of the CDR script')
        t.column('cps-script', 'character', format='x(20)', initial='',
                 label='CPS Script',
                 column_label='CPS Script',
                 help='Name of the CPS script')
        t.column('Reseller', 'integer', format='99', initial='0',
                 label='Code',
                 column_label='Code',
                 help='Reseller code')
        t.column('cli-pref', 'character', format='x(4)', initial='',
                 label='Prefix',
                 column_label='Prefix',
                 help='Partners prefix')
        t.column('rs-dir', 'character', format='x(20)', initial='',
                 label='Home directory',
                 column_label='Home directory',
                 help='Home directory, ie. whereunder the subdirectories are')
        t.column('pstype', 'integer', format='9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Type of presel. 0):None 1):Nat 2):Intn\'l 3):Both',
                 valexp='INPUT pstype < 4',
                 valmsg='Type of preselction MUST be 0 ... 3 !')
        t.column('res-script', 'character', format='x(20)', initial='',
                 label='RES Script',
                 column_label='RES Script',
                 help='Name of the response script')
        t.column('wl-fileid', 'character', format='x(4)', initial='',
                 label='FileID',
                 column_label='FileID',
                 help='File identification prefix')
        t.column('Interval', 'integer', format='z9', initial='0',
                 column_label='Int.',
                 help='Interval for files in hours')
        t.column('CDRSeq', 'integer', format='>>>>>>9', initial='0',
                 label='CDR Seq',
                 column_label='CDR Seq',
                 help='CDR file sequence number')
        t.index('CustNum', ['CustNum', 'Operator'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('interval', ['Interval', 'fileid'], area='Sta_Index_2')
        t.index('Operator', ['Operator', 'CustNum'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('rsoper')

