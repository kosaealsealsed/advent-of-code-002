INSERT INTO advent_of_code_002.remove_entry_setting (
    remove_entry_option_id,
    date_created,
    date_updated,
    remove_entry_setting_id
)
    SELECT
        remove_entry_option_id,
        date_created,
        date_updated,
        remove_entry_setting_id
    FROM
        journal_table_002.remove_entry_setting;