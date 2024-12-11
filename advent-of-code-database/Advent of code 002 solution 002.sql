CREATE TABLE SOLUTION_002 (
    SOLUTION_002           CLOB,
    date_created           TIMESTAMP(9) WITH TIME ZONE DEFAULT systimestamp(9) NOT NULL,
    date_updated           TIMESTAMP(9) WITH TIME ZONE,
    date_created_or_updated TIMESTAMP(9) WITH TIME ZONE GENERATED ALWAYS AS ( coalesce(date_updated, date_created) ) VIRTUAL,
    SOLUTION_002_id         RAW(16) DEFAULT sys_guid() PRIMARY KEY
);

-- Trigger to update date_updated for SOLUTION_002
CREATE OR REPLACE TRIGGER set_date_updated_SOLUTION_002
    BEFORE UPDATE ON SOLUTION_002
    FOR EACH ROW
BEGIN
    :new.date_updated := systimestamp;
END;
/
