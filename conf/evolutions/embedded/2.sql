# --- !Ups

alter table "users" drop column "compact_entry_list";
alter table "users" add "entry_list_type" integer default 0;

# --- !Downs

alter table "users" drop column "entry_list_type";
alter table "users" add "compact_entry_list" boolean default false not null;