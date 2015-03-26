from gearbox.migrations import Migration

class AddOrder(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('Order', area='Sta_Data_32',
                       dump_name='order')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='CustNum',
                 help='Reference customer number',
                 description='Reference customer number')
        t.column('Source', 'character', initial='',
                 column_label='Source',
                 help='Source of order',
                 description='Source of order')
        t.column('CLI', 'character', format='x(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('CLIType', 'character', initial='',
                 column_label='CLIType',
                 help='CLI Type',
                 description='CLI Type')
        t.column('CredOK', 'logical', initial='no',
                 label='Credit',
                 column_label='Credit',
                 help='Is the customers credit ok?')
        t.column('StatusCode', 'character', initial='',
                 label='Status',
                 column_label='Status',
                 help='Status of order',
                 description='Status of order')
        t.column('CrStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='When was the order created',
                 description='Create timestamp')
        t.column('ContactNum', 'character', format='x(12)', initial='',
                 column_label='ContactNum',
                 description='Contact Phone number')
        t.column('MNPStatus', 'integer', format='99', initial='0',
                 column_label='MNPStatus',
                 help='Status of MNP')
        t.column('CurrOper', 'character', format='x(15)', initial='',
                 label='Current Operator',
                 column_label='Current Operator')
        t.column('MNPNumber', 'character', format='x(12)', initial='',
                 label='MNP Number',
                 column_label='MNP Number')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Code of that salesman who is responsible of this reseller')
        t.column('Tupas', 'integer', format='>9', initial='0',
                 column_label='Tupas')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('TupasRM', 'character', format='x(200)', initial='',
                 column_label='TupasRM',
                 help='Tupas Return Message',
                 description='Tupas Return Message')
        t.column('CreditRate', 'character', initial='',
                 label='Credit Rating',
                 column_label='CrRate',
                 help='Credit rating class')
        t.column('CREventQty', 'integer', format='>>9', initial='0',
                 label='Credit Event Qty',
                 column_label='CREvent Qty',
                 help='Credit event quantity')
        t.column('DepoFeeAmt', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Deposit Fee Amount',
                 column_label='DepoFee',
                 help='Deposit fee amount')
        t.column('Foreign', 'logical', initial='no',
                 help='Foreign user')
        t.column('InhabitClass', 'integer', format='9', initial='0',
                 label='Inhabit Class',
                 column_label='Inhabit',
                 help='Inhabit class')
        t.column('ClaimState', 'integer', format='>9', initial='0',
                 label='In Claiming',
                 column_label='Claimed',
                 help='Customer has invoices in claiming process')
        t.column('PNPNumbers', 'character', format='x(50)', initial='',
                 label='PNP Numbers',
                 column_label='PNP',
                 help='PNP numbers')
        t.column('OrderType', 'integer', format='9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Order type')
        t.column('ICC', 'character', format='x(20)', initial='',
                 column_label='ICC',
                 help='SIM card serial no.',
                 description='SIM card serial no.')
        t.column('LeadNbr', 'integer', format='>>>>>>>9', initial='0',
                 label='Lead Number',
                 column_label='Lead',
                 help='Lead number')
        t.column('OrderChannel', 'character', format='x(16)', initial='',
                 label='Order Channel',
                 column_label='Channel',
                 help='Order channel')
        t.column('MSSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='MobSub Sequence',
                 column_label='MobSub',
                 help='Link to MobSub')
        t.column('Campaign', 'character', initial='',
                 label='Campaign ID',
                 column_label='ID')
        t.column('PortingDate', 'date', format='99-99-99',
                 column_label='PortingDate',
                 help='Porting date')
        t.column('Referee', 'character', format='x(16)', initial='',
                 column_label='Ref',
                 help='Referee\'s CLI')
        t.column('PortingTime', 'decimal', decimals=2, format='99.99', initial='0',
                 column_label='PortingTime')
        t.column('FeeModel', 'character', initial='',
                 label='BEvent',
                 column_label='FeeModel',
                 help='An unique code for a Billing Event')
        t.column('PayType', 'logical', format='PrePaid/PostPaid', initial='no',
                 column_label='PayType')
        t.column('OldIcc', 'character', format='x(20)', initial='',
                 label='Old ICC',
                 column_label='Old ICC')
        t.column('DeviceID', 'character', format='x(16)', initial='',
                 column_label='DeviceID')
        t.column('TempCLI', 'character', format='x(11)', initial='',
                 label='Temp MSISDN',
                 column_label='Temp MSISDN',
                 help='Temporary MSISDN Subscriber No')
        t.column('InvCustRole', 'integer', format='9', initial='1',
                 label='Inv.Customer Role',
                 column_label='ICRole',
                 help='Invoice customer role')
        t.column('UserRole', 'integer', format='9', initial='1',
                 label='User Customer Role',
                 column_label='UserRole',
                 help='User customer role')
        t.column('Reseller', 'character', initial='',
                 column_label='Resell',
                 help='Reseller code')
        t.column('OrdererIP', 'character', format='x(20)', initial='',
                 label='Orderer IP',
                 column_label='Ord.IP')
        t.column('ResellerMobNbr', 'character', format='x(14)', initial='',
                 label='Reseller Mobile Number',
                 column_label='Res.Mobile',
                 help='Reseller mobile number')
        t.column('OldPayType', 'logical', format='PrePaid/PostPaid', initial='no',
                 label='Old Pay Type',
                 column_label='Old PT',
                 help='Old pay type')
        t.column('Orderer', 'character', format='x(30)', initial='')
        t.column('OrdererIDType', 'character', initial='',
                 label='Orderer ID Type',
                 column_label='Ord.IDType',
                 help='Orderer ID type')
        t.column('OrdererID', 'character', format='x(10)', initial='',
                 label='Orderer ID',
                 column_label='Ord.ID')
        t.column('InvNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Invoice Nbr',
                 column_label='InvNum',
                 help='Invoice number')
        t.column('ContractID', 'character', format='x(12)', initial='',
                 label='Contract ID',
                 column_label='ContrID')
        t.column('FATAmount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='FAT Amount',
                 column_label='FAT',
                 help='FATime amount')
        t.column('SMSType', 'integer', format='>9', initial='0',
                 label='SMS Type',
                 column_label='SMS Type')
        t.column('Offer', 'character', format='x(12)', initial='',
                 label='Offer ID',
                 column_label='Offer')
        t.column('Logistics', 'character', format='x(30)', initial='',
                 column_label='Logistics',
                 help='Logistics file name')
        t.column('FtGrp', 'character', initial='',
                 label='FATime Group',
                 column_label='FATGroup',
                 help='FATime group')
        t.column('ROIResult', 'character', format='x(20)', initial='',
                 label='ROI Result',
                 column_label='ROI',
                 help='Result code from ROI')
        t.column('SendToROI', 'integer', format='9', initial='0',
                 label='Send History to ROI')
        t.column('RiskCode', 'character', format='x(10)', initial='',
                 label='ROI Risk Code')
        t.index('CLI', ['Brand', 'CLI', ('CrStamp', 'DESCENDING'), 'OrderType', 'Tupas'], area='Sta_Index_1')
        t.index('CLI_s', ['CLI', ('CrStamp', 'DESCENDING')], area='Sta_Index_1')
        t.index('ContractID', ['Brand', 'ContractID'], area='Sta_Index_1')
        t.index('CustNum', ['Brand', 'CustNum', ('CrStamp', 'DESCENDING')], area='Sta_Index_1')
        t.index('CustNum_s', ['CustNum', ('CrStamp', 'DESCENDING')], area='Sta_Index_1')
        t.index('InvNum', ['InvNum'], area='Sta_Index_1')
        t.index('MSSeq', ['MSSeq'], area='Sta_Index_1')
        t.index('OrderID', ['Brand', 'OrderId'], area='Sta_Index_1',
                primary=True, unique=True)
        t.index('OrderIDBr', ['Brand', 'OrderId', 'OrderType', 'Tupas'], area='Sta_Index_1')
        t.index('SendToROI', ['Brand', 'SendToROI'], area='Sta_Index_1')
        t.index('Source', ['Brand', 'Source', ('CrStamp', 'DESCENDING')], area='Sta_Index_1')
        t.index('Stamp', ['Brand', ('CrStamp', 'DESCENDING'), 'OrderType', 'Tupas'], area='Sta_Index_1')
        t.index('StatusCLI', ['Brand', 'StatusCode', 'CLI'], area='Sta_Index_1')
        t.index('StatusCode', ['Brand', 'StatusCode', ('CrStamp', 'DESCENDING')], area='Sta_Index_1')
        t.index('StContractID', ['Brand', 'StatusCode', 'ContractID'], area='Sta_Index_1')
        t.index('StOrderID', ['Brand', 'StatusCode', 'OrderId'], area='Sta_Index_1')

    def down(self):
        self.drop_table('Order')

