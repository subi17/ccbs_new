from gearbox.migrations import Migration

class AddOfferCriteria(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OfferCriteria', area='Sta_Data_128',
                       label='Offer Criteria',
                       dump_name='offercriteria',
                       desc='Offer criteria')
        t.column('Offer', 'character', format='x(12)', initial='',
                 label='Offer ID',
                 column_label='Offer')
        t.column('CriteriaType', 'character', format='x(20)', initial='',
                 label='Criteria Type',
                 column_label='Type',
                 help='Criteria type')
        t.column('IncludedValue', 'character', format='x(30)', initial='',
                 label='Included Value',
                 column_label='Included',
                 help='Values that are included in this offer')
        t.column('BeginStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('EndStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('ExcludedValue', 'character', format='x(30)', initial='',
                 label='Excluded Value',
                 column_label='Excluded',
                 help='Values that are excluded from this offer')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('OfferCriteriaID', 'integer', format='>>>>>>>>9', initial='0',
                 label='Criteria ID',
                 column_label='ID',
                 help='Offer criteria ID')
        t.index('Criteria', ['Brand', 'Offer', 'OfferCriteriaID'], area='Sta_Index_2',
                primary=True)
        t.index('CriteriaType', ['Brand', 'CriteriaType', ('EndStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('EndStamp', ['Brand', ('EndStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('Offer', ['Brand', 'Offer', ('EndStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('OfferCriteriaID', ['OfferCriteriaID'], area='Sta_Index_2',
                unique=True)
        t.index('OfferCriteriaType', ['Brand', 'Offer', 'CriteriaType', ('BeginStamp', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('OfferCriteria')

