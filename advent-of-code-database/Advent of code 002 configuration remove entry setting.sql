CREATE TABLE REMOVE_ENTRY_SETTING (
    REMOVE_ENTRY_OPTION_id raw(16) references REMOVE_ENTRY_OPTION(REMOVE_ENTRY_OPTION_id),
    date_created           TIMESTAMP(9) WITH TIME ZONE DEFAULT systimestamp(9) NOT NULL,
    date_updated           TIMESTAMP(9) WITH TIME ZONE,
    date_created_or_updated TIMESTAMP(9) WITH TIME ZONE GENERATED ALWAYS AS ( coalesce(date_updated, date_created) ) VIRTUAL,
    REMOVE_ENTRY_SETTING_id         RAW(16) DEFAULT sys_guid() PRIMARY KEY
);

-- Trigger to update date_updated for REMOVE_ENTRY_SETTING
CREATE OR REPLACE TRIGGER set_date_updated_REMOVE_ENTRY_SETTING
    BEFORE UPDATE ON REMOVE_ENTRY_SETTING
    FOR EACH ROW
BEGIN
    :new.date_updated := systimestamp;
END;
/
