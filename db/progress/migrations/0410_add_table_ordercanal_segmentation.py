from gearbox.migrations import Migration

class AddTableSegmentation(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('Segmentation', area="Sta_Data_128", dump_name="segmentation", desc="Collect segmentation information")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="MsSeq", column_label="MsSeq", position=2, order=10, help="Subs. ID")
        t.column('SegmentCode', 'character', format="x(8)", initial="", max_width=16, label="Segmentation Code", column_label="SegmentCode", position=3, order=20, help="Segmentation code")
        t.column('SegmentOffer', 'character', format="x(8)", initial="", max_width=16, label="Segmentation Offer", column_label="SegmentOffer", position=4, order=30, help="Segmentation Offer")
        t.column('SegmentDate', 'date', format="99-99-99", max_width=4, label="Segmentation Date", column_label="SegmentDate", position=5, order=40, help="Date when segmentation code was changed")
        t.column('SegmentCons', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="SegmentCons", column_label="SegmentCons", position=6, order=50, help="Average consumption in euros")
        t.column('SegmentCreation', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation time", column_label="Creation time", position=7, order=60, help="Creation time of segmentation code")
        t.index('MsSeqDate', [['MsSeq'], ['SegmentDate', 'DESC']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('Segmentation')
