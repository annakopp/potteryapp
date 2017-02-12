-- Revert potteryapp:appschema from pg

BEGIN;

  DROP SCHEMA potteryapp;

COMMIT;
