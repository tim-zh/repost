# --- !Ups

create table "users" (
	"id" BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
	"version" BIGINT DEFAULT 0 NOT NULL,
	"name" VARCHAR NOT NULL,
	"password" VARCHAR NOT NULL,
	"compact_entry_list" BOOLEAN DEFAULT false NOT NULL,
	"date_format" VARCHAR DEFAULT 'dd MMM yyyy HH:mm:ss' NOT NULL,
	"items_on_page" INTEGER DEFAULT 10 NOT NULL,
	"code_theme" INTEGER DEFAULT 0 NOT NULL);
create unique index "idx_user_name" on "users" ("name");
create unique index "idx_user_name_password" on "users" ("name", "password");
create table "entries" (
	"id" BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
	"version" BIGINT DEFAULT 0 NOT NULL,
	"author_id" BIGINT NOT NULL,
	"title" VARCHAR NOT NULL,
	"content" varchar(40960) NOT NULL,
	"date" TIMESTAMP NOT NULL,
	"open_for_all" BOOLEAN NOT NULL);
create index "idx_entry_title" on "entries" ("title");
create table "comments" (
	"id" BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
	"version" BIGINT DEFAULT 0 NOT NULL,
	"author_id" BIGINT NOT NULL,
	"date" TIMESTAMP NOT NULL,
	"content" varchar(4096) NOT NULL,
	"entry_id" BIGINT NOT NULL);
create table "tags" (
	"id" BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
	"version" BIGINT DEFAULT 0 NOT NULL,
	"title" VARCHAR NOT NULL);
create index "idx_tag_title" on "tags" ("title");
create table "user_tag" (
	"userid" BIGINT NOT NULL,
	"tagid" BIGINT NOT NULL);
create table "entry_tag" (
	"entryid" BIGINT NOT NULL,
	"tagid" BIGINT NOT NULL);
alter table "entries" add constraint "fk_entry_author" foreign key("author_id") references "users"("id") on update NO ACTION on delete NO ACTION;
alter table "comments" add constraint "fk_comment_author" foreign key("author_id") references "users"("id") on update NO ACTION on delete NO ACTION;
alter table "comments" add constraint "fk_comment_entry" foreign key("entry_id") references "entries"("id") on update NO ACTION on delete NO ACTION;
alter table "user_tag" add constraint "fk_user_favorite_tag_user" foreign key("userid") references "users"("id") on update NO ACTION on delete NO ACTION;
alter table "user_tag" add constraint "fk_user_favorite_tag_tag" foreign key("tagid") references "tags"("id") on update NO ACTION on delete NO ACTION;
alter table "entry_tag" add constraint "fk_entry_tag_entry" foreign key("entryid") references "entries"("id") on update NO ACTION on delete NO ACTION;
alter table "entry_tag" add constraint "fk_entry_tag_tag" foreign key("tagid") references "tags"("id") on update NO ACTION on delete NO ACTION;

# --- !Downs

drop table "users";
drop table "entries";
drop table "comments";
drop table "tags";
drop table "user_tag";
drop table "entry_tag";