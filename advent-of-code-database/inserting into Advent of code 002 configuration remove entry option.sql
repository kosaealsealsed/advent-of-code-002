INSERT INTO advent_of_code_002.remove_entry_option (
    remove_entry_option,
    date_created,
    date_updated,
    remove_entry_option_id
)
    SELECT
        remove_entry_option,
        date_created,
        date_updated,
        remove_entry_option_id
    FROM
        journal_table_002.remove_entry_option;