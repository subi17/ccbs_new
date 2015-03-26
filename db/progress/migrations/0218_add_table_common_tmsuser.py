from gearbox.migrations import Migration

class AddTMSUser(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSUser', area='Sta_Data_32',
                       label='Users',
                       dump_name='tmsuser',
                       desc='Users')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User ID',
                 column_label='User ID',
                 help='User ID, 1 - 8 characters')
        t.column('Password', 'character', initial='',
                 label='Passwrd',
                 column_label='Passwrd',
                 help='Password, 1 - 8 characters')
        t.column('UserGroup', 'character', format='x(10)', initial='',
                 label='GroupCode',
                 column_label='GroupCode',
                 help='Individual Code for a User Group')
        t.column('UserNum', 'integer', mandatory=True, format='999', initial='0',
                 label='UserNo',
                 column_label='UserNo',
                 help='User\'s internal consecutive number',
                 description='Käyttäjätunn. liittyvä numeerinen yksilöintiavain')
        t.column('UserName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Entire name of user')
        t.column('Active', 'logical', format='Kyllä/Ei', initial='yes',
                 label='Activ',
                 column_label='Activ',
                 help='Active')
        t.column('Initials', 'character', format='x(3)', initial='',
                 label='Sort',
                 column_label='Sort',
                 help='User initials ')
        t.column('StartMenu', 'character', format='x(5)', initial='',
                 label='GroupNo',
                 column_label='GroupNo',
                 help='Salesgroup\'s ID')
        t.column('WorkStation', 'logical', format='Mustavalko/Väri', initial='no',
                 label='Workst.',
                 column_label='Workst.',
                 help='Is a workstation black&white or colour')
        t.column('Modem', 'character', format='x(10)', initial='',
                 column_label='Modem',
                 help='Location of the user\'s modem')
        t.column('NetCode', 'character', format='x(10)', initial='',
                 label='NetNo',
                 column_label='NetNo',
                 help='Net code')
        t.column('Prefix', 'character', format='x(10)', initial='',
                 label='Foreigh',
                 column_label='Foreigh',
                 help='Prefix for outgoing international calls')
        t.column('DialCmd', 'character', format='x(10)', initial='',
                 column_label='DialCmd',
                 help='Dialing Command')
        t.column('RepDir', 'character', format='x(30)', initial='',
                 label='Txt-directory',
                 column_label='Txt-directory',
                 help='Default output  dirctory of .txt files')
        t.column('Email', 'character', format='x(40)', initial='',
                 label='E-mail',
                 column_label='E-mail',
                 help='Customer\'s e-mail address')
        t.column('Brand', 'character', format='x(40)', initial='',
                 column_label='Brand',
                 help='Code Of Brands')
        t.column('CreditLimit', 'decimal', decimals=2, format='>>>>>>>>>9.99', initial='0',
                 label='Crediting Limit',
                 column_label='CreditLimit',
                 help='Max. invoice amount that this user can credit')
        t.column('ErgoKeyb', 'logical', initial='no',
                 label='ErgoKbd',
                 column_label='ErgoKeyboard',
                 help='Is this Erconomy keyboard')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('ForeignId', 'character', format='x(20)', initial='',
                 column_label='ForeignId')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Date from',
                 column_label='Date from',
                 help='Activation date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Date to',
                 column_label='Date to',
                 help='Expiring date')
        t.column('TopUpLimit', 'integer', format='999', initial='75',
                 column_label='TopUpLimit',
                 help='Max amount TMS-user can give TopUp')
        t.column('AdminUser', 'logical', initial='no',
                 label='Admin User',
                 column_label='Admin',
                 help='Admin level user')
        t.index('ForeignId', ['ForeignId', 'UserCode', 'UserName'], area='Sta_Index_2')
        t.index('UserCode', ['UserCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('UserGroup', ['UserGroup'], area='Sta_Index_2')
        t.index('UserName', ['UserName', 'UserCode'], area='Sta_Index_2',
                unique=True)
        t.index('UserNum', ['UserNum'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('TMSUser')

