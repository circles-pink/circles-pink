/* eslint-disable */

import { AllTypesProps, ReturnTypes } from './const';
type ZEUS_INTERFACES = never
type ZEUS_UNIONS = never

export type ValueTypes = {
    ["Query"]: AliasType<{
names?: [{	filter?:ValueTypes["names_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["names"]],
names_by_id?: [{	id:string},ValueTypes["names"]],
names_aggregated?: [{	groupBy?:(string | undefined | null)[],	filter?:ValueTypes["names_filter"] | null,	limit?:number | null,	search?:string | null,	sort?:(string | undefined | null)[]},ValueTypes["names_aggregated"]],
views?: [{	filter?:ValueTypes["views_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["views"]],
views_by_id?: [{	id:string},ValueTypes["views"]],
views_aggregated?: [{	groupBy?:(string | undefined | null)[],	filter?:ValueTypes["views_filter"] | null,	limit?:number | null,	search?:string | null,	sort?:(string | undefined | null)[]},ValueTypes["views_aggregated"]],
views_translations?: [{	filter?:ValueTypes["views_translations_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["views_translations"]],
views_translations_by_id?: [{	id:string},ValueTypes["views_translations"]],
views_translations_aggregated?: [{	groupBy?:(string | undefined | null)[],	filter?:ValueTypes["views_translations_filter"] | null,	limit?:number | null,	search?:string | null,	sort?:(string | undefined | null)[]},ValueTypes["views_translations_aggregated"]],
languages?: [{	filter?:ValueTypes["languages_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["languages"]],
languages_by_id?: [{	id:string},ValueTypes["languages"]],
		__typename?: boolean
}>;
	["names"]: AliasType<{
	id?:boolean,
	name?:boolean,
		__typename?: boolean
}>;
	["names_filter"]: {
	id?:ValueTypes["number_filter_operators"] | null,
	name?:ValueTypes["string_filter_operators"] | null,
	_and?:(ValueTypes["names_filter"] | undefined | null)[],
	_or?:(ValueTypes["names_filter"] | undefined | null)[]
};
	["number_filter_operators"]: {
	_eq?:number | null,
	_neq?:number | null,
	_in?:(number | undefined | null)[],
	_nin?:(number | undefined | null)[],
	_gt?:number | null,
	_gte?:number | null,
	_lt?:number | null,
	_lte?:number | null,
	_null?:boolean | null,
	_nnull?:boolean | null
};
	["string_filter_operators"]: {
	_eq?:string | null,
	_neq?:string | null,
	_contains?:string | null,
	_ncontains?:string | null,
	_starts_with?:string | null,
	_nstarts_with?:string | null,
	_ends_with?:string | null,
	_nends_with?:string | null,
	_in?:(string | undefined | null)[],
	_nin?:(string | undefined | null)[],
	_null?:boolean | null,
	_nnull?:boolean | null,
	_empty?:boolean | null,
	_nempty?:boolean | null
};
	["names_aggregated"]: AliasType<{
	group?:boolean,
	avg?:ValueTypes["names_aggregated_fields"],
	sum?:ValueTypes["names_aggregated_fields"],
	count?:ValueTypes["names_aggregated_fields"],
	countDistinct?:ValueTypes["names_aggregated_fields"],
	avgDistinct?:ValueTypes["names_aggregated_fields"],
	sumDistinct?:ValueTypes["names_aggregated_fields"],
	min?:ValueTypes["names_aggregated_fields"],
	max?:ValueTypes["names_aggregated_fields"],
		__typename?: boolean
}>;
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:unknown;
	["names_aggregated_fields"]: AliasType<{
	id?:boolean,
		__typename?: boolean
}>;
	["views"]: AliasType<{
user_created?: [{	filter?:ValueTypes["directus_users_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_users"]],
user_updated?: [{	filter?:ValueTypes["directus_users_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_users"]],
	id?:boolean,
	status?:boolean,
	sort?:boolean,
	date_created?:boolean,
	date_created_func?:ValueTypes["datetime_functions"],
	date_updated?:boolean,
	date_updated_func?:ValueTypes["datetime_functions"],
	enum?:boolean,
name?: [{	filter?:ValueTypes["names_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["names"]],
translations?: [{	filter?:ValueTypes["views_translations_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["views_translations"]],
		__typename?: boolean
}>;
	["directus_users"]: AliasType<{
	id?:boolean,
	first_name?:boolean,
	last_name?:boolean,
	email?:boolean,
	password?:boolean,
	location?:boolean,
	title?:boolean,
	description?:boolean,
	tags?:boolean,
avatar?: [{	filter?:ValueTypes["directus_files_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_files"]],
	language?:boolean,
	theme?:boolean,
	tfa_secret?:boolean,
	status?:boolean,
role?: [{	filter?:ValueTypes["directus_roles_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_roles"]],
	token?:boolean,
	last_access?:boolean,
	last_access_func?:ValueTypes["datetime_functions"],
	last_page?:boolean,
	provider?:boolean,
	external_identifier?:boolean,
	auth_data?:boolean,
	email_notifications?:boolean,
		__typename?: boolean
}>;
	["directus_files"]: AliasType<{
	id?:boolean,
	storage?:boolean,
	filename_disk?:boolean,
	filename_download?:boolean,
	title?:boolean,
	type?:boolean,
folder?: [{	filter?:ValueTypes["directus_folders_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_folders"]],
uploaded_by?: [{	filter?:ValueTypes["directus_users_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_users"]],
	uploaded_on?:boolean,
	uploaded_on_func?:ValueTypes["datetime_functions"],
modified_by?: [{	filter?:ValueTypes["directus_users_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_users"]],
	modified_on?:boolean,
	modified_on_func?:ValueTypes["datetime_functions"],
	charset?:boolean,
	filesize?:boolean,
	width?:boolean,
	height?:boolean,
	duration?:boolean,
	embed?:boolean,
	description?:boolean,
	location?:boolean,
	tags?:boolean,
	metadata?:boolean,
		__typename?: boolean
}>;
	["directus_folders"]: AliasType<{
	id?:boolean,
	name?:boolean,
parent?: [{	filter?:ValueTypes["directus_folders_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_folders"]],
		__typename?: boolean
}>;
	["directus_folders_filter"]: {
	id?:ValueTypes["string_filter_operators"] | null,
	name?:ValueTypes["string_filter_operators"] | null,
	parent?:ValueTypes["directus_folders_filter"] | null,
	_and?:(ValueTypes["directus_folders_filter"] | undefined | null)[],
	_or?:(ValueTypes["directus_folders_filter"] | undefined | null)[]
};
	["directus_users_filter"]: {
	id?:ValueTypes["string_filter_operators"] | null,
	first_name?:ValueTypes["string_filter_operators"] | null,
	last_name?:ValueTypes["string_filter_operators"] | null,
	email?:ValueTypes["string_filter_operators"] | null,
	password?:ValueTypes["string_filter_operators"] | null,
	location?:ValueTypes["string_filter_operators"] | null,
	title?:ValueTypes["string_filter_operators"] | null,
	description?:ValueTypes["string_filter_operators"] | null,
	tags?:ValueTypes["string_filter_operators"] | null,
	avatar?:ValueTypes["directus_files_filter"] | null,
	language?:ValueTypes["string_filter_operators"] | null,
	theme?:ValueTypes["string_filter_operators"] | null,
	tfa_secret?:ValueTypes["string_filter_operators"] | null,
	status?:ValueTypes["string_filter_operators"] | null,
	role?:ValueTypes["directus_roles_filter"] | null,
	token?:ValueTypes["string_filter_operators"] | null,
	last_access?:ValueTypes["date_filter_operators"] | null,
	last_access_func?:ValueTypes["datetime_function_filter_operators"] | null,
	last_page?:ValueTypes["string_filter_operators"] | null,
	provider?:ValueTypes["string_filter_operators"] | null,
	external_identifier?:ValueTypes["string_filter_operators"] | null,
	auth_data?:ValueTypes["string_filter_operators"] | null,
	email_notifications?:ValueTypes["boolean_filter_operators"] | null,
	_and?:(ValueTypes["directus_users_filter"] | undefined | null)[],
	_or?:(ValueTypes["directus_users_filter"] | undefined | null)[]
};
	["directus_files_filter"]: {
	id?:ValueTypes["string_filter_operators"] | null,
	storage?:ValueTypes["string_filter_operators"] | null,
	filename_disk?:ValueTypes["string_filter_operators"] | null,
	filename_download?:ValueTypes["string_filter_operators"] | null,
	title?:ValueTypes["string_filter_operators"] | null,
	type?:ValueTypes["string_filter_operators"] | null,
	folder?:ValueTypes["directus_folders_filter"] | null,
	uploaded_by?:ValueTypes["directus_users_filter"] | null,
	uploaded_on?:ValueTypes["date_filter_operators"] | null,
	uploaded_on_func?:ValueTypes["datetime_function_filter_operators"] | null,
	modified_by?:ValueTypes["directus_users_filter"] | null,
	modified_on?:ValueTypes["date_filter_operators"] | null,
	modified_on_func?:ValueTypes["datetime_function_filter_operators"] | null,
	charset?:ValueTypes["string_filter_operators"] | null,
	filesize?:ValueTypes["number_filter_operators"] | null,
	width?:ValueTypes["number_filter_operators"] | null,
	height?:ValueTypes["number_filter_operators"] | null,
	duration?:ValueTypes["number_filter_operators"] | null,
	embed?:ValueTypes["string_filter_operators"] | null,
	description?:ValueTypes["string_filter_operators"] | null,
	location?:ValueTypes["string_filter_operators"] | null,
	tags?:ValueTypes["string_filter_operators"] | null,
	metadata?:ValueTypes["string_filter_operators"] | null,
	_and?:(ValueTypes["directus_files_filter"] | undefined | null)[],
	_or?:(ValueTypes["directus_files_filter"] | undefined | null)[]
};
	["date_filter_operators"]: {
	_eq?:string | null,
	_neq?:string | null,
	_gt?:string | null,
	_gte?:string | null,
	_lt?:string | null,
	_lte?:string | null,
	_null?:boolean | null,
	_nnull?:boolean | null
};
	["datetime_function_filter_operators"]: {
	year?:ValueTypes["number_filter_operators"] | null,
	month?:ValueTypes["number_filter_operators"] | null,
	week?:ValueTypes["number_filter_operators"] | null,
	day?:ValueTypes["number_filter_operators"] | null,
	weekday?:ValueTypes["number_filter_operators"] | null,
	hour?:ValueTypes["number_filter_operators"] | null,
	minute?:ValueTypes["number_filter_operators"] | null,
	second?:ValueTypes["number_filter_operators"] | null
};
	["directus_roles_filter"]: {
	id?:ValueTypes["string_filter_operators"] | null,
	name?:ValueTypes["string_filter_operators"] | null,
	icon?:ValueTypes["string_filter_operators"] | null,
	description?:ValueTypes["string_filter_operators"] | null,
	ip_access?:ValueTypes["string_filter_operators"] | null,
	enforce_tfa?:ValueTypes["boolean_filter_operators"] | null,
	admin_access?:ValueTypes["boolean_filter_operators"] | null,
	app_access?:ValueTypes["boolean_filter_operators"] | null,
	users?:ValueTypes["directus_users_filter"] | null,
	_and?:(ValueTypes["directus_roles_filter"] | undefined | null)[],
	_or?:(ValueTypes["directus_roles_filter"] | undefined | null)[]
};
	["boolean_filter_operators"]: {
	_eq?:boolean | null,
	_neq?:boolean | null,
	_null?:boolean | null,
	_nnull?:boolean | null
};
	/** ISO8601 Date values */
["Date"]:unknown;
	["datetime_functions"]: AliasType<{
	year?:boolean,
	month?:boolean,
	week?:boolean,
	day?:boolean,
	weekday?:boolean,
	hour?:boolean,
	minute?:boolean,
	second?:boolean,
		__typename?: boolean
}>;
	["directus_roles"]: AliasType<{
	id?:boolean,
	name?:boolean,
	icon?:boolean,
	description?:boolean,
	ip_access?:boolean,
	enforce_tfa?:boolean,
	admin_access?:boolean,
	app_access?:boolean,
users?: [{	filter?:ValueTypes["directus_users_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["directus_users"]],
		__typename?: boolean
}>;
	["views_translations"]: AliasType<{
	id?:boolean,
views_id?: [{	filter?:ValueTypes["views_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["views"]],
languages_id?: [{	filter?:ValueTypes["languages_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null},ValueTypes["languages"]],
	foo?:boolean,
		__typename?: boolean
}>;
	["views_filter"]: {
	user_created?:ValueTypes["directus_users_filter"] | null,
	user_updated?:ValueTypes["directus_users_filter"] | null,
	id?:ValueTypes["string_filter_operators"] | null,
	status?:ValueTypes["string_filter_operators"] | null,
	sort?:ValueTypes["number_filter_operators"] | null,
	date_created?:ValueTypes["date_filter_operators"] | null,
	date_created_func?:ValueTypes["datetime_function_filter_operators"] | null,
	date_updated?:ValueTypes["date_filter_operators"] | null,
	date_updated_func?:ValueTypes["datetime_function_filter_operators"] | null,
	enum?:ValueTypes["string_filter_operators"] | null,
	name?:ValueTypes["names_filter"] | null,
	translations?:ValueTypes["views_translations_filter"] | null,
	_and?:(ValueTypes["views_filter"] | undefined | null)[],
	_or?:(ValueTypes["views_filter"] | undefined | null)[]
};
	["views_translations_filter"]: {
	id?:ValueTypes["number_filter_operators"] | null,
	views_id?:ValueTypes["views_filter"] | null,
	languages_id?:ValueTypes["languages_filter"] | null,
	foo?:ValueTypes["string_filter_operators"] | null,
	_and?:(ValueTypes["views_translations_filter"] | undefined | null)[],
	_or?:(ValueTypes["views_translations_filter"] | undefined | null)[]
};
	["languages_filter"]: {
	code?:ValueTypes["string_filter_operators"] | null,
	name?:ValueTypes["string_filter_operators"] | null,
	_and?:(ValueTypes["languages_filter"] | undefined | null)[],
	_or?:(ValueTypes["languages_filter"] | undefined | null)[]
};
	["languages"]: AliasType<{
	code?:boolean,
	name?:boolean,
		__typename?: boolean
}>;
	["views_aggregated"]: AliasType<{
	group?:boolean,
	avg?:ValueTypes["views_aggregated_fields"],
	sum?:ValueTypes["views_aggregated_fields"],
	count?:ValueTypes["views_aggregated_fields"],
	countDistinct?:ValueTypes["views_aggregated_fields"],
	avgDistinct?:ValueTypes["views_aggregated_fields"],
	sumDistinct?:ValueTypes["views_aggregated_fields"],
	min?:ValueTypes["views_aggregated_fields"],
	max?:ValueTypes["views_aggregated_fields"],
		__typename?: boolean
}>;
	["views_aggregated_fields"]: AliasType<{
	sort?:boolean,
	name?:boolean,
		__typename?: boolean
}>;
	["views_translations_aggregated"]: AliasType<{
	group?:boolean,
	avg?:ValueTypes["views_translations_aggregated_fields"],
	sum?:ValueTypes["views_translations_aggregated_fields"],
	count?:ValueTypes["views_translations_aggregated_fields"],
	countDistinct?:ValueTypes["views_translations_aggregated_fields"],
	avgDistinct?:ValueTypes["views_translations_aggregated_fields"],
	sumDistinct?:ValueTypes["views_translations_aggregated_fields"],
	min?:ValueTypes["views_translations_aggregated_fields"],
	max?:ValueTypes["views_translations_aggregated_fields"],
		__typename?: boolean
}>;
	["views_translations_aggregated_fields"]: AliasType<{
	id?:boolean,
		__typename?: boolean
}>;
	["Mutation"]: AliasType<{
create_names_items?: [{	filter?:ValueTypes["names_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	data?:ValueTypes["create_names_input"][]},ValueTypes["names"]],
create_names_item?: [{	data:ValueTypes["create_names_input"]},ValueTypes["names"]],
create_views_items?: [{	filter?:ValueTypes["views_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	data?:ValueTypes["create_views_input"][]},ValueTypes["views"]],
create_views_item?: [{	data:ValueTypes["create_views_input"]},ValueTypes["views"]],
create_views_translations_items?: [{	filter?:ValueTypes["views_translations_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	data?:ValueTypes["create_views_translations_input"][]},ValueTypes["views_translations"]],
create_views_translations_item?: [{	data:ValueTypes["create_views_translations_input"]},ValueTypes["views_translations"]],
create_languages_items?: [{	filter?:ValueTypes["languages_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	data?:ValueTypes["create_languages_input"][]},ValueTypes["languages"]],
create_languages_item?: [{	data:ValueTypes["create_languages_input"]},ValueTypes["languages"]],
update_names_items?: [{	filter?:ValueTypes["names_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	ids?:string[],	data:ValueTypes["update_names_input"]},ValueTypes["names"]],
update_names_item?: [{	id:string,	data:ValueTypes["update_names_input"]},ValueTypes["names"]],
update_views_items?: [{	filter?:ValueTypes["views_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	ids?:string[],	data:ValueTypes["update_views_input"]},ValueTypes["views"]],
update_views_item?: [{	id:string,	data:ValueTypes["update_views_input"]},ValueTypes["views"]],
update_views_translations_items?: [{	filter?:ValueTypes["views_translations_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	ids?:string[],	data:ValueTypes["update_views_translations_input"]},ValueTypes["views_translations"]],
update_views_translations_item?: [{	id:string,	data:ValueTypes["update_views_translations_input"]},ValueTypes["views_translations"]],
update_languages_items?: [{	filter?:ValueTypes["languages_filter"] | null,	sort?:(string | undefined | null)[],	limit?:number | null,	offset?:number | null,	page?:number | null,	search?:string | null,	ids?:string[],	data:ValueTypes["update_languages_input"]},ValueTypes["languages"]],
update_languages_item?: [{	id:string,	data:ValueTypes["update_languages_input"]},ValueTypes["languages"]],
delete_names_items?: [{	ids?:string[]},ValueTypes["delete_many"]],
delete_names_item?: [{	id:string},ValueTypes["delete_one"]],
delete_views_items?: [{	ids?:string[]},ValueTypes["delete_many"]],
delete_views_item?: [{	id:string},ValueTypes["delete_one"]],
delete_views_translations_items?: [{	ids?:string[]},ValueTypes["delete_many"]],
delete_views_translations_item?: [{	id:string},ValueTypes["delete_one"]],
delete_languages_items?: [{	ids?:string[]},ValueTypes["delete_many"]],
delete_languages_item?: [{	id:string},ValueTypes["delete_one"]],
		__typename?: boolean
}>;
	["create_names_input"]: {
	id?:string | null,
	name:string
};
	["create_views_input"]: {
	user_created?:ValueTypes["create_directus_users_input"] | null,
	user_updated?:ValueTypes["create_directus_users_input"] | null,
	id?:string | null,
	status:string,
	sort?:number | null,
	date_created?:ValueTypes["Date"] | null,
	date_created_func?:ValueTypes["datetime_functionsInput"] | null,
	date_updated?:ValueTypes["Date"] | null,
	date_updated_func?:ValueTypes["datetime_functionsInput"] | null,
	enum:string,
	name?:ValueTypes["create_names_input"] | null,
	translations?:(ValueTypes["create_views_translations_input"] | undefined | null)[]
};
	["create_directus_users_input"]: {
	id?:string | null,
	first_name?:string | null,
	last_name?:string | null,
	email?:string | null,
	password?:string | null,
	location?:string | null,
	title?:string | null,
	description?:string | null,
	tags?:ValueTypes["JSON"] | null,
	avatar?:ValueTypes["create_directus_files_input"] | null,
	language?:string | null,
	theme?:string | null,
	tfa_secret?:string | null,
	status:string,
	role?:ValueTypes["create_directus_roles_input"] | null,
	token?:string | null,
	last_access:ValueTypes["Date"],
	last_access_func?:ValueTypes["datetime_functionsInput"] | null,
	last_page?:string | null,
	provider:string,
	external_identifier?:string | null,
	auth_data?:string | null,
	email_notifications?:boolean | null
};
	["create_directus_files_input"]: {
	id?:string | null,
	storage:string,
	filename_disk?:string | null,
	filename_download:string,
	title?:string | null,
	type?:string | null,
	folder?:ValueTypes["create_directus_folders_input"] | null,
	uploaded_by?:ValueTypes["create_directus_users_input"] | null,
	uploaded_on:ValueTypes["Date"],
	uploaded_on_func?:ValueTypes["datetime_functionsInput"] | null,
	modified_by?:ValueTypes["create_directus_users_input"] | null,
	modified_on:ValueTypes["Date"],
	modified_on_func?:ValueTypes["datetime_functionsInput"] | null,
	charset?:string | null,
	filesize?:number | null,
	width?:number | null,
	height?:number | null,
	duration?:number | null,
	embed?:string | null,
	description?:string | null,
	location?:string | null,
	tags?:ValueTypes["JSON"] | null,
	metadata?:ValueTypes["JSON"] | null
};
	["create_directus_folders_input"]: {
	id?:string | null,
	name:string,
	parent?:ValueTypes["create_directus_folders_input"] | null
};
	["datetime_functionsInput"]: {
	year?:number | null,
	month?:number | null,
	week?:number | null,
	day?:number | null,
	weekday?:number | null,
	hour?:number | null,
	minute?:number | null,
	second?:number | null
};
	["create_directus_roles_input"]: {
	id?:string | null,
	name:string,
	icon:string,
	description?:string | null,
	ip_access?:(string | undefined | null)[],
	enforce_tfa:boolean,
	admin_access:boolean,
	app_access:boolean,
	users?:(ValueTypes["create_directus_users_input"] | undefined | null)[]
};
	["create_views_translations_input"]: {
	id?:string | null,
	views_id?:ValueTypes["create_views_input"] | null,
	languages_id?:ValueTypes["create_languages_input"] | null,
	foo:string
};
	["create_languages_input"]: {
	code?:string | null,
	name?:string | null
};
	["update_names_input"]: {
	id?:string | null,
	name?:string | null
};
	["update_views_input"]: {
	user_created?:ValueTypes["update_directus_users_input"] | null,
	user_updated?:ValueTypes["update_directus_users_input"] | null,
	id?:string | null,
	status?:string | null,
	sort?:number | null,
	date_created?:ValueTypes["Date"] | null,
	date_created_func?:ValueTypes["datetime_functionsInput"] | null,
	date_updated?:ValueTypes["Date"] | null,
	date_updated_func?:ValueTypes["datetime_functionsInput"] | null,
	enum?:string | null,
	name?:ValueTypes["update_names_input"] | null,
	translations?:(ValueTypes["update_views_translations_input"] | undefined | null)[]
};
	["update_directus_users_input"]: {
	id?:string | null,
	first_name?:string | null,
	last_name?:string | null,
	email?:string | null,
	password?:string | null,
	location?:string | null,
	title?:string | null,
	description?:string | null,
	tags?:ValueTypes["JSON"] | null,
	avatar?:ValueTypes["update_directus_files_input"] | null,
	language?:string | null,
	theme?:string | null,
	tfa_secret?:string | null,
	status?:string | null,
	role?:ValueTypes["update_directus_roles_input"] | null,
	token?:string | null,
	last_access?:ValueTypes["Date"] | null,
	last_access_func?:ValueTypes["datetime_functionsInput"] | null,
	last_page?:string | null,
	provider?:string | null,
	external_identifier?:string | null,
	auth_data?:string | null,
	email_notifications?:boolean | null
};
	["update_directus_files_input"]: {
	id?:string | null,
	storage?:string | null,
	filename_disk?:string | null,
	filename_download?:string | null,
	title?:string | null,
	type?:string | null,
	folder?:ValueTypes["update_directus_folders_input"] | null,
	uploaded_by?:ValueTypes["update_directus_users_input"] | null,
	uploaded_on?:ValueTypes["Date"] | null,
	uploaded_on_func?:ValueTypes["datetime_functionsInput"] | null,
	modified_by?:ValueTypes["update_directus_users_input"] | null,
	modified_on?:ValueTypes["Date"] | null,
	modified_on_func?:ValueTypes["datetime_functionsInput"] | null,
	charset?:string | null,
	filesize?:number | null,
	width?:number | null,
	height?:number | null,
	duration?:number | null,
	embed?:string | null,
	description?:string | null,
	location?:string | null,
	tags?:ValueTypes["JSON"] | null,
	metadata?:ValueTypes["JSON"] | null
};
	["update_directus_folders_input"]: {
	id?:string | null,
	name?:string | null,
	parent?:ValueTypes["update_directus_folders_input"] | null
};
	["update_directus_roles_input"]: {
	id?:string | null,
	name?:string | null,
	icon?:string | null,
	description?:string | null,
	ip_access?:(string | undefined | null)[],
	enforce_tfa?:boolean | null,
	admin_access?:boolean | null,
	app_access?:boolean | null,
	users?:(ValueTypes["update_directus_users_input"] | undefined | null)[]
};
	["update_views_translations_input"]: {
	id?:string | null,
	views_id?:ValueTypes["update_views_input"] | null,
	languages_id?:ValueTypes["update_languages_input"] | null,
	foo?:string | null
};
	["update_languages_input"]: {
	code?:string | null,
	name?:string | null
};
	["delete_many"]: AliasType<{
	ids?:boolean,
		__typename?: boolean
}>;
	["delete_one"]: AliasType<{
	id?:boolean,
		__typename?: boolean
}>
  }

export type ModelTypes = {
    ["Query"]: {
		names?:(ModelTypes["names"] | undefined)[],
	names_by_id?:ModelTypes["names"],
	names_aggregated?:(ModelTypes["names_aggregated"] | undefined)[],
	views?:(ModelTypes["views"] | undefined)[],
	views_by_id?:ModelTypes["views"],
	views_aggregated?:(ModelTypes["views_aggregated"] | undefined)[],
	views_translations?:(ModelTypes["views_translations"] | undefined)[],
	views_translations_by_id?:ModelTypes["views_translations"],
	views_translations_aggregated?:(ModelTypes["views_translations_aggregated"] | undefined)[],
	languages?:(ModelTypes["languages"] | undefined)[],
	languages_by_id?:ModelTypes["languages"]
};
	["names"]: {
		id?:string,
	name:string
};
	["names_filter"]: GraphQLTypes["names_filter"];
	["number_filter_operators"]: GraphQLTypes["number_filter_operators"];
	["string_filter_operators"]: GraphQLTypes["string_filter_operators"];
	["names_aggregated"]: {
		group?:ModelTypes["JSON"],
	avg?:ModelTypes["names_aggregated_fields"],
	sum?:ModelTypes["names_aggregated_fields"],
	count?:ModelTypes["names_aggregated_fields"],
	countDistinct?:ModelTypes["names_aggregated_fields"],
	avgDistinct?:ModelTypes["names_aggregated_fields"],
	sumDistinct?:ModelTypes["names_aggregated_fields"],
	min?:ModelTypes["names_aggregated_fields"],
	max?:ModelTypes["names_aggregated_fields"]
};
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:any;
	["names_aggregated_fields"]: {
		id?:number
};
	["views"]: {
		user_created?:ModelTypes["directus_users"],
	user_updated?:ModelTypes["directus_users"],
	id?:string,
	status:string,
	sort?:number,
	date_created?:ModelTypes["Date"],
	date_created_func?:ModelTypes["datetime_functions"],
	date_updated?:ModelTypes["Date"],
	date_updated_func?:ModelTypes["datetime_functions"],
	enum:string,
	name?:ModelTypes["names"],
	translations?:(ModelTypes["views_translations"] | undefined)[]
};
	["directus_users"]: {
		id?:string,
	first_name?:string,
	last_name?:string,
	email?:string,
	password?:string,
	location?:string,
	title?:string,
	description?:string,
	tags?:ModelTypes["JSON"],
	avatar?:ModelTypes["directus_files"],
	language?:string,
	theme?:string,
	tfa_secret?:string,
	status:string,
	role?:ModelTypes["directus_roles"],
	token?:string,
	last_access:ModelTypes["Date"],
	last_access_func?:ModelTypes["datetime_functions"],
	last_page?:string,
	provider:string,
	external_identifier?:string,
	auth_data?:string,
	email_notifications?:boolean
};
	["directus_files"]: {
		id?:string,
	storage:string,
	filename_disk?:string,
	filename_download:string,
	title?:string,
	type?:string,
	folder?:ModelTypes["directus_folders"],
	uploaded_by?:ModelTypes["directus_users"],
	uploaded_on:ModelTypes["Date"],
	uploaded_on_func?:ModelTypes["datetime_functions"],
	modified_by?:ModelTypes["directus_users"],
	modified_on:ModelTypes["Date"],
	modified_on_func?:ModelTypes["datetime_functions"],
	charset?:string,
	filesize?:number,
	width?:number,
	height?:number,
	duration?:number,
	embed?:string,
	description?:string,
	location?:string,
	tags?:ModelTypes["JSON"],
	metadata?:ModelTypes["JSON"]
};
	["directus_folders"]: {
		id?:string,
	name:string,
	parent?:ModelTypes["directus_folders"]
};
	["directus_folders_filter"]: GraphQLTypes["directus_folders_filter"];
	["directus_users_filter"]: GraphQLTypes["directus_users_filter"];
	["directus_files_filter"]: GraphQLTypes["directus_files_filter"];
	["date_filter_operators"]: GraphQLTypes["date_filter_operators"];
	["datetime_function_filter_operators"]: GraphQLTypes["datetime_function_filter_operators"];
	["directus_roles_filter"]: GraphQLTypes["directus_roles_filter"];
	["boolean_filter_operators"]: GraphQLTypes["boolean_filter_operators"];
	/** ISO8601 Date values */
["Date"]:any;
	["datetime_functions"]: {
		year?:number,
	month?:number,
	week?:number,
	day?:number,
	weekday?:number,
	hour?:number,
	minute?:number,
	second?:number
};
	["directus_roles"]: {
		id?:string,
	name:string,
	icon:string,
	description?:string,
	ip_access?:(string | undefined)[],
	enforce_tfa:boolean,
	admin_access:boolean,
	app_access:boolean,
	users?:(ModelTypes["directus_users"] | undefined)[]
};
	["views_translations"]: {
		id?:string,
	views_id?:ModelTypes["views"],
	languages_id?:ModelTypes["languages"],
	foo:string
};
	["views_filter"]: GraphQLTypes["views_filter"];
	["views_translations_filter"]: GraphQLTypes["views_translations_filter"];
	["languages_filter"]: GraphQLTypes["languages_filter"];
	["languages"]: {
		code?:string,
	name?:string
};
	["views_aggregated"]: {
		group?:ModelTypes["JSON"],
	avg?:ModelTypes["views_aggregated_fields"],
	sum?:ModelTypes["views_aggregated_fields"],
	count?:ModelTypes["views_aggregated_fields"],
	countDistinct?:ModelTypes["views_aggregated_fields"],
	avgDistinct?:ModelTypes["views_aggregated_fields"],
	sumDistinct?:ModelTypes["views_aggregated_fields"],
	min?:ModelTypes["views_aggregated_fields"],
	max?:ModelTypes["views_aggregated_fields"]
};
	["views_aggregated_fields"]: {
		sort?:number,
	name?:number
};
	["views_translations_aggregated"]: {
		group?:ModelTypes["JSON"],
	avg?:ModelTypes["views_translations_aggregated_fields"],
	sum?:ModelTypes["views_translations_aggregated_fields"],
	count?:ModelTypes["views_translations_aggregated_fields"],
	countDistinct?:ModelTypes["views_translations_aggregated_fields"],
	avgDistinct?:ModelTypes["views_translations_aggregated_fields"],
	sumDistinct?:ModelTypes["views_translations_aggregated_fields"],
	min?:ModelTypes["views_translations_aggregated_fields"],
	max?:ModelTypes["views_translations_aggregated_fields"]
};
	["views_translations_aggregated_fields"]: {
		id?:number
};
	["Mutation"]: {
		create_names_items?:(ModelTypes["names"] | undefined)[],
	create_names_item?:ModelTypes["names"],
	create_views_items?:(ModelTypes["views"] | undefined)[],
	create_views_item?:ModelTypes["views"],
	create_views_translations_items?:(ModelTypes["views_translations"] | undefined)[],
	create_views_translations_item?:ModelTypes["views_translations"],
	create_languages_items?:(ModelTypes["languages"] | undefined)[],
	create_languages_item?:ModelTypes["languages"],
	update_names_items?:(ModelTypes["names"] | undefined)[],
	update_names_item?:ModelTypes["names"],
	update_views_items?:(ModelTypes["views"] | undefined)[],
	update_views_item?:ModelTypes["views"],
	update_views_translations_items?:(ModelTypes["views_translations"] | undefined)[],
	update_views_translations_item?:ModelTypes["views_translations"],
	update_languages_items?:(ModelTypes["languages"] | undefined)[],
	update_languages_item?:ModelTypes["languages"],
	delete_names_items?:ModelTypes["delete_many"],
	delete_names_item?:ModelTypes["delete_one"],
	delete_views_items?:ModelTypes["delete_many"],
	delete_views_item?:ModelTypes["delete_one"],
	delete_views_translations_items?:ModelTypes["delete_many"],
	delete_views_translations_item?:ModelTypes["delete_one"],
	delete_languages_items?:ModelTypes["delete_many"],
	delete_languages_item?:ModelTypes["delete_one"]
};
	["create_names_input"]: GraphQLTypes["create_names_input"];
	["create_views_input"]: GraphQLTypes["create_views_input"];
	["create_directus_users_input"]: GraphQLTypes["create_directus_users_input"];
	["create_directus_files_input"]: GraphQLTypes["create_directus_files_input"];
	["create_directus_folders_input"]: GraphQLTypes["create_directus_folders_input"];
	["datetime_functionsInput"]: GraphQLTypes["datetime_functionsInput"];
	["create_directus_roles_input"]: GraphQLTypes["create_directus_roles_input"];
	["create_views_translations_input"]: GraphQLTypes["create_views_translations_input"];
	["create_languages_input"]: GraphQLTypes["create_languages_input"];
	["update_names_input"]: GraphQLTypes["update_names_input"];
	["update_views_input"]: GraphQLTypes["update_views_input"];
	["update_directus_users_input"]: GraphQLTypes["update_directus_users_input"];
	["update_directus_files_input"]: GraphQLTypes["update_directus_files_input"];
	["update_directus_folders_input"]: GraphQLTypes["update_directus_folders_input"];
	["update_directus_roles_input"]: GraphQLTypes["update_directus_roles_input"];
	["update_views_translations_input"]: GraphQLTypes["update_views_translations_input"];
	["update_languages_input"]: GraphQLTypes["update_languages_input"];
	["delete_many"]: {
		ids?:string[]
};
	["delete_one"]: {
		id:string
}
    }

export type GraphQLTypes = {
    ["Query"]: {
	__typename: "Query",
	names?: Array<GraphQLTypes["names"] | undefined>,
	names_by_id?: GraphQLTypes["names"],
	names_aggregated?: Array<GraphQLTypes["names_aggregated"] | undefined>,
	views?: Array<GraphQLTypes["views"] | undefined>,
	views_by_id?: GraphQLTypes["views"],
	views_aggregated?: Array<GraphQLTypes["views_aggregated"] | undefined>,
	views_translations?: Array<GraphQLTypes["views_translations"] | undefined>,
	views_translations_by_id?: GraphQLTypes["views_translations"],
	views_translations_aggregated?: Array<GraphQLTypes["views_translations_aggregated"] | undefined>,
	languages?: Array<GraphQLTypes["languages"] | undefined>,
	languages_by_id?: GraphQLTypes["languages"]
};
	["names"]: {
	__typename: "names",
	id?: string,
	name: string
};
	["names_filter"]: {
		id?: GraphQLTypes["number_filter_operators"],
	name?: GraphQLTypes["string_filter_operators"],
	_and?: Array<GraphQLTypes["names_filter"] | undefined>,
	_or?: Array<GraphQLTypes["names_filter"] | undefined>
};
	["number_filter_operators"]: {
		_eq?: number,
	_neq?: number,
	_in?: Array<number | undefined>,
	_nin?: Array<number | undefined>,
	_gt?: number,
	_gte?: number,
	_lt?: number,
	_lte?: number,
	_null?: boolean,
	_nnull?: boolean
};
	["string_filter_operators"]: {
		_eq?: string,
	_neq?: string,
	_contains?: string,
	_ncontains?: string,
	_starts_with?: string,
	_nstarts_with?: string,
	_ends_with?: string,
	_nends_with?: string,
	_in?: Array<string | undefined>,
	_nin?: Array<string | undefined>,
	_null?: boolean,
	_nnull?: boolean,
	_empty?: boolean,
	_nempty?: boolean
};
	["names_aggregated"]: {
	__typename: "names_aggregated",
	group?: GraphQLTypes["JSON"],
	avg?: GraphQLTypes["names_aggregated_fields"],
	sum?: GraphQLTypes["names_aggregated_fields"],
	count?: GraphQLTypes["names_aggregated_fields"],
	countDistinct?: GraphQLTypes["names_aggregated_fields"],
	avgDistinct?: GraphQLTypes["names_aggregated_fields"],
	sumDistinct?: GraphQLTypes["names_aggregated_fields"],
	min?: GraphQLTypes["names_aggregated_fields"],
	max?: GraphQLTypes["names_aggregated_fields"]
};
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:any;
	["names_aggregated_fields"]: {
	__typename: "names_aggregated_fields",
	id?: number
};
	["views"]: {
	__typename: "views",
	user_created?: GraphQLTypes["directus_users"],
	user_updated?: GraphQLTypes["directus_users"],
	id?: string,
	status: string,
	sort?: number,
	date_created?: GraphQLTypes["Date"],
	date_created_func?: GraphQLTypes["datetime_functions"],
	date_updated?: GraphQLTypes["Date"],
	date_updated_func?: GraphQLTypes["datetime_functions"],
	enum: string,
	name?: GraphQLTypes["names"],
	translations?: Array<GraphQLTypes["views_translations"] | undefined>
};
	["directus_users"]: {
	__typename: "directus_users",
	id?: string,
	first_name?: string,
	last_name?: string,
	email?: string,
	password?: string,
	location?: string,
	title?: string,
	description?: string,
	tags?: GraphQLTypes["JSON"],
	avatar?: GraphQLTypes["directus_files"],
	language?: string,
	theme?: string,
	tfa_secret?: string,
	status: string,
	role?: GraphQLTypes["directus_roles"],
	token?: string,
	last_access: GraphQLTypes["Date"],
	last_access_func?: GraphQLTypes["datetime_functions"],
	last_page?: string,
	provider: string,
	external_identifier?: string,
	auth_data?: string,
	email_notifications?: boolean
};
	["directus_files"]: {
	__typename: "directus_files",
	id?: string,
	storage: string,
	filename_disk?: string,
	filename_download: string,
	title?: string,
	type?: string,
	folder?: GraphQLTypes["directus_folders"],
	uploaded_by?: GraphQLTypes["directus_users"],
	uploaded_on: GraphQLTypes["Date"],
	uploaded_on_func?: GraphQLTypes["datetime_functions"],
	modified_by?: GraphQLTypes["directus_users"],
	modified_on: GraphQLTypes["Date"],
	modified_on_func?: GraphQLTypes["datetime_functions"],
	charset?: string,
	filesize?: number,
	width?: number,
	height?: number,
	duration?: number,
	embed?: string,
	description?: string,
	location?: string,
	tags?: GraphQLTypes["JSON"],
	metadata?: GraphQLTypes["JSON"]
};
	["directus_folders"]: {
	__typename: "directus_folders",
	id?: string,
	name: string,
	parent?: GraphQLTypes["directus_folders"]
};
	["directus_folders_filter"]: {
		id?: GraphQLTypes["string_filter_operators"],
	name?: GraphQLTypes["string_filter_operators"],
	parent?: GraphQLTypes["directus_folders_filter"],
	_and?: Array<GraphQLTypes["directus_folders_filter"] | undefined>,
	_or?: Array<GraphQLTypes["directus_folders_filter"] | undefined>
};
	["directus_users_filter"]: {
		id?: GraphQLTypes["string_filter_operators"],
	first_name?: GraphQLTypes["string_filter_operators"],
	last_name?: GraphQLTypes["string_filter_operators"],
	email?: GraphQLTypes["string_filter_operators"],
	password?: GraphQLTypes["string_filter_operators"],
	location?: GraphQLTypes["string_filter_operators"],
	title?: GraphQLTypes["string_filter_operators"],
	description?: GraphQLTypes["string_filter_operators"],
	tags?: GraphQLTypes["string_filter_operators"],
	avatar?: GraphQLTypes["directus_files_filter"],
	language?: GraphQLTypes["string_filter_operators"],
	theme?: GraphQLTypes["string_filter_operators"],
	tfa_secret?: GraphQLTypes["string_filter_operators"],
	status?: GraphQLTypes["string_filter_operators"],
	role?: GraphQLTypes["directus_roles_filter"],
	token?: GraphQLTypes["string_filter_operators"],
	last_access?: GraphQLTypes["date_filter_operators"],
	last_access_func?: GraphQLTypes["datetime_function_filter_operators"],
	last_page?: GraphQLTypes["string_filter_operators"],
	provider?: GraphQLTypes["string_filter_operators"],
	external_identifier?: GraphQLTypes["string_filter_operators"],
	auth_data?: GraphQLTypes["string_filter_operators"],
	email_notifications?: GraphQLTypes["boolean_filter_operators"],
	_and?: Array<GraphQLTypes["directus_users_filter"] | undefined>,
	_or?: Array<GraphQLTypes["directus_users_filter"] | undefined>
};
	["directus_files_filter"]: {
		id?: GraphQLTypes["string_filter_operators"],
	storage?: GraphQLTypes["string_filter_operators"],
	filename_disk?: GraphQLTypes["string_filter_operators"],
	filename_download?: GraphQLTypes["string_filter_operators"],
	title?: GraphQLTypes["string_filter_operators"],
	type?: GraphQLTypes["string_filter_operators"],
	folder?: GraphQLTypes["directus_folders_filter"],
	uploaded_by?: GraphQLTypes["directus_users_filter"],
	uploaded_on?: GraphQLTypes["date_filter_operators"],
	uploaded_on_func?: GraphQLTypes["datetime_function_filter_operators"],
	modified_by?: GraphQLTypes["directus_users_filter"],
	modified_on?: GraphQLTypes["date_filter_operators"],
	modified_on_func?: GraphQLTypes["datetime_function_filter_operators"],
	charset?: GraphQLTypes["string_filter_operators"],
	filesize?: GraphQLTypes["number_filter_operators"],
	width?: GraphQLTypes["number_filter_operators"],
	height?: GraphQLTypes["number_filter_operators"],
	duration?: GraphQLTypes["number_filter_operators"],
	embed?: GraphQLTypes["string_filter_operators"],
	description?: GraphQLTypes["string_filter_operators"],
	location?: GraphQLTypes["string_filter_operators"],
	tags?: GraphQLTypes["string_filter_operators"],
	metadata?: GraphQLTypes["string_filter_operators"],
	_and?: Array<GraphQLTypes["directus_files_filter"] | undefined>,
	_or?: Array<GraphQLTypes["directus_files_filter"] | undefined>
};
	["date_filter_operators"]: {
		_eq?: string,
	_neq?: string,
	_gt?: string,
	_gte?: string,
	_lt?: string,
	_lte?: string,
	_null?: boolean,
	_nnull?: boolean
};
	["datetime_function_filter_operators"]: {
		year?: GraphQLTypes["number_filter_operators"],
	month?: GraphQLTypes["number_filter_operators"],
	week?: GraphQLTypes["number_filter_operators"],
	day?: GraphQLTypes["number_filter_operators"],
	weekday?: GraphQLTypes["number_filter_operators"],
	hour?: GraphQLTypes["number_filter_operators"],
	minute?: GraphQLTypes["number_filter_operators"],
	second?: GraphQLTypes["number_filter_operators"]
};
	["directus_roles_filter"]: {
		id?: GraphQLTypes["string_filter_operators"],
	name?: GraphQLTypes["string_filter_operators"],
	icon?: GraphQLTypes["string_filter_operators"],
	description?: GraphQLTypes["string_filter_operators"],
	ip_access?: GraphQLTypes["string_filter_operators"],
	enforce_tfa?: GraphQLTypes["boolean_filter_operators"],
	admin_access?: GraphQLTypes["boolean_filter_operators"],
	app_access?: GraphQLTypes["boolean_filter_operators"],
	users?: GraphQLTypes["directus_users_filter"],
	_and?: Array<GraphQLTypes["directus_roles_filter"] | undefined>,
	_or?: Array<GraphQLTypes["directus_roles_filter"] | undefined>
};
	["boolean_filter_operators"]: {
		_eq?: boolean,
	_neq?: boolean,
	_null?: boolean,
	_nnull?: boolean
};
	/** ISO8601 Date values */
["Date"]:any;
	["datetime_functions"]: {
	__typename: "datetime_functions",
	year?: number,
	month?: number,
	week?: number,
	day?: number,
	weekday?: number,
	hour?: number,
	minute?: number,
	second?: number
};
	["directus_roles"]: {
	__typename: "directus_roles",
	id?: string,
	name: string,
	icon: string,
	description?: string,
	ip_access?: Array<string | undefined>,
	enforce_tfa: boolean,
	admin_access: boolean,
	app_access: boolean,
	users?: Array<GraphQLTypes["directus_users"] | undefined>
};
	["views_translations"]: {
	__typename: "views_translations",
	id?: string,
	views_id?: GraphQLTypes["views"],
	languages_id?: GraphQLTypes["languages"],
	foo: string
};
	["views_filter"]: {
		user_created?: GraphQLTypes["directus_users_filter"],
	user_updated?: GraphQLTypes["directus_users_filter"],
	id?: GraphQLTypes["string_filter_operators"],
	status?: GraphQLTypes["string_filter_operators"],
	sort?: GraphQLTypes["number_filter_operators"],
	date_created?: GraphQLTypes["date_filter_operators"],
	date_created_func?: GraphQLTypes["datetime_function_filter_operators"],
	date_updated?: GraphQLTypes["date_filter_operators"],
	date_updated_func?: GraphQLTypes["datetime_function_filter_operators"],
	enum?: GraphQLTypes["string_filter_operators"],
	name?: GraphQLTypes["names_filter"],
	translations?: GraphQLTypes["views_translations_filter"],
	_and?: Array<GraphQLTypes["views_filter"] | undefined>,
	_or?: Array<GraphQLTypes["views_filter"] | undefined>
};
	["views_translations_filter"]: {
		id?: GraphQLTypes["number_filter_operators"],
	views_id?: GraphQLTypes["views_filter"],
	languages_id?: GraphQLTypes["languages_filter"],
	foo?: GraphQLTypes["string_filter_operators"],
	_and?: Array<GraphQLTypes["views_translations_filter"] | undefined>,
	_or?: Array<GraphQLTypes["views_translations_filter"] | undefined>
};
	["languages_filter"]: {
		code?: GraphQLTypes["string_filter_operators"],
	name?: GraphQLTypes["string_filter_operators"],
	_and?: Array<GraphQLTypes["languages_filter"] | undefined>,
	_or?: Array<GraphQLTypes["languages_filter"] | undefined>
};
	["languages"]: {
	__typename: "languages",
	code?: string,
	name?: string
};
	["views_aggregated"]: {
	__typename: "views_aggregated",
	group?: GraphQLTypes["JSON"],
	avg?: GraphQLTypes["views_aggregated_fields"],
	sum?: GraphQLTypes["views_aggregated_fields"],
	count?: GraphQLTypes["views_aggregated_fields"],
	countDistinct?: GraphQLTypes["views_aggregated_fields"],
	avgDistinct?: GraphQLTypes["views_aggregated_fields"],
	sumDistinct?: GraphQLTypes["views_aggregated_fields"],
	min?: GraphQLTypes["views_aggregated_fields"],
	max?: GraphQLTypes["views_aggregated_fields"]
};
	["views_aggregated_fields"]: {
	__typename: "views_aggregated_fields",
	sort?: number,
	name?: number
};
	["views_translations_aggregated"]: {
	__typename: "views_translations_aggregated",
	group?: GraphQLTypes["JSON"],
	avg?: GraphQLTypes["views_translations_aggregated_fields"],
	sum?: GraphQLTypes["views_translations_aggregated_fields"],
	count?: GraphQLTypes["views_translations_aggregated_fields"],
	countDistinct?: GraphQLTypes["views_translations_aggregated_fields"],
	avgDistinct?: GraphQLTypes["views_translations_aggregated_fields"],
	sumDistinct?: GraphQLTypes["views_translations_aggregated_fields"],
	min?: GraphQLTypes["views_translations_aggregated_fields"],
	max?: GraphQLTypes["views_translations_aggregated_fields"]
};
	["views_translations_aggregated_fields"]: {
	__typename: "views_translations_aggregated_fields",
	id?: number
};
	["Mutation"]: {
	__typename: "Mutation",
	create_names_items?: Array<GraphQLTypes["names"] | undefined>,
	create_names_item?: GraphQLTypes["names"],
	create_views_items?: Array<GraphQLTypes["views"] | undefined>,
	create_views_item?: GraphQLTypes["views"],
	create_views_translations_items?: Array<GraphQLTypes["views_translations"] | undefined>,
	create_views_translations_item?: GraphQLTypes["views_translations"],
	create_languages_items?: Array<GraphQLTypes["languages"] | undefined>,
	create_languages_item?: GraphQLTypes["languages"],
	update_names_items?: Array<GraphQLTypes["names"] | undefined>,
	update_names_item?: GraphQLTypes["names"],
	update_views_items?: Array<GraphQLTypes["views"] | undefined>,
	update_views_item?: GraphQLTypes["views"],
	update_views_translations_items?: Array<GraphQLTypes["views_translations"] | undefined>,
	update_views_translations_item?: GraphQLTypes["views_translations"],
	update_languages_items?: Array<GraphQLTypes["languages"] | undefined>,
	update_languages_item?: GraphQLTypes["languages"],
	delete_names_items?: GraphQLTypes["delete_many"],
	delete_names_item?: GraphQLTypes["delete_one"],
	delete_views_items?: GraphQLTypes["delete_many"],
	delete_views_item?: GraphQLTypes["delete_one"],
	delete_views_translations_items?: GraphQLTypes["delete_many"],
	delete_views_translations_item?: GraphQLTypes["delete_one"],
	delete_languages_items?: GraphQLTypes["delete_many"],
	delete_languages_item?: GraphQLTypes["delete_one"]
};
	["create_names_input"]: {
		id?: string,
	name: string
};
	["create_views_input"]: {
		user_created?: GraphQLTypes["create_directus_users_input"],
	user_updated?: GraphQLTypes["create_directus_users_input"],
	id?: string,
	status: string,
	sort?: number,
	date_created?: GraphQLTypes["Date"],
	date_created_func?: GraphQLTypes["datetime_functionsInput"],
	date_updated?: GraphQLTypes["Date"],
	date_updated_func?: GraphQLTypes["datetime_functionsInput"],
	enum: string,
	name?: GraphQLTypes["create_names_input"],
	translations?: Array<GraphQLTypes["create_views_translations_input"] | undefined>
};
	["create_directus_users_input"]: {
		id?: string,
	first_name?: string,
	last_name?: string,
	email?: string,
	password?: string,
	location?: string,
	title?: string,
	description?: string,
	tags?: GraphQLTypes["JSON"],
	avatar?: GraphQLTypes["create_directus_files_input"],
	language?: string,
	theme?: string,
	tfa_secret?: string,
	status: string,
	role?: GraphQLTypes["create_directus_roles_input"],
	token?: string,
	last_access: GraphQLTypes["Date"],
	last_access_func?: GraphQLTypes["datetime_functionsInput"],
	last_page?: string,
	provider: string,
	external_identifier?: string,
	auth_data?: string,
	email_notifications?: boolean
};
	["create_directus_files_input"]: {
		id?: string,
	storage: string,
	filename_disk?: string,
	filename_download: string,
	title?: string,
	type?: string,
	folder?: GraphQLTypes["create_directus_folders_input"],
	uploaded_by?: GraphQLTypes["create_directus_users_input"],
	uploaded_on: GraphQLTypes["Date"],
	uploaded_on_func?: GraphQLTypes["datetime_functionsInput"],
	modified_by?: GraphQLTypes["create_directus_users_input"],
	modified_on: GraphQLTypes["Date"],
	modified_on_func?: GraphQLTypes["datetime_functionsInput"],
	charset?: string,
	filesize?: number,
	width?: number,
	height?: number,
	duration?: number,
	embed?: string,
	description?: string,
	location?: string,
	tags?: GraphQLTypes["JSON"],
	metadata?: GraphQLTypes["JSON"]
};
	["create_directus_folders_input"]: {
		id?: string,
	name: string,
	parent?: GraphQLTypes["create_directus_folders_input"]
};
	["datetime_functionsInput"]: {
		year?: number,
	month?: number,
	week?: number,
	day?: number,
	weekday?: number,
	hour?: number,
	minute?: number,
	second?: number
};
	["create_directus_roles_input"]: {
		id?: string,
	name: string,
	icon: string,
	description?: string,
	ip_access?: Array<string | undefined>,
	enforce_tfa: boolean,
	admin_access: boolean,
	app_access: boolean,
	users?: Array<GraphQLTypes["create_directus_users_input"] | undefined>
};
	["create_views_translations_input"]: {
		id?: string,
	views_id?: GraphQLTypes["create_views_input"],
	languages_id?: GraphQLTypes["create_languages_input"],
	foo: string
};
	["create_languages_input"]: {
		code?: string,
	name?: string
};
	["update_names_input"]: {
		id?: string,
	name?: string
};
	["update_views_input"]: {
		user_created?: GraphQLTypes["update_directus_users_input"],
	user_updated?: GraphQLTypes["update_directus_users_input"],
	id?: string,
	status?: string,
	sort?: number,
	date_created?: GraphQLTypes["Date"],
	date_created_func?: GraphQLTypes["datetime_functionsInput"],
	date_updated?: GraphQLTypes["Date"],
	date_updated_func?: GraphQLTypes["datetime_functionsInput"],
	enum?: string,
	name?: GraphQLTypes["update_names_input"],
	translations?: Array<GraphQLTypes["update_views_translations_input"] | undefined>
};
	["update_directus_users_input"]: {
		id?: string,
	first_name?: string,
	last_name?: string,
	email?: string,
	password?: string,
	location?: string,
	title?: string,
	description?: string,
	tags?: GraphQLTypes["JSON"],
	avatar?: GraphQLTypes["update_directus_files_input"],
	language?: string,
	theme?: string,
	tfa_secret?: string,
	status?: string,
	role?: GraphQLTypes["update_directus_roles_input"],
	token?: string,
	last_access?: GraphQLTypes["Date"],
	last_access_func?: GraphQLTypes["datetime_functionsInput"],
	last_page?: string,
	provider?: string,
	external_identifier?: string,
	auth_data?: string,
	email_notifications?: boolean
};
	["update_directus_files_input"]: {
		id?: string,
	storage?: string,
	filename_disk?: string,
	filename_download?: string,
	title?: string,
	type?: string,
	folder?: GraphQLTypes["update_directus_folders_input"],
	uploaded_by?: GraphQLTypes["update_directus_users_input"],
	uploaded_on?: GraphQLTypes["Date"],
	uploaded_on_func?: GraphQLTypes["datetime_functionsInput"],
	modified_by?: GraphQLTypes["update_directus_users_input"],
	modified_on?: GraphQLTypes["Date"],
	modified_on_func?: GraphQLTypes["datetime_functionsInput"],
	charset?: string,
	filesize?: number,
	width?: number,
	height?: number,
	duration?: number,
	embed?: string,
	description?: string,
	location?: string,
	tags?: GraphQLTypes["JSON"],
	metadata?: GraphQLTypes["JSON"]
};
	["update_directus_folders_input"]: {
		id?: string,
	name?: string,
	parent?: GraphQLTypes["update_directus_folders_input"]
};
	["update_directus_roles_input"]: {
		id?: string,
	name?: string,
	icon?: string,
	description?: string,
	ip_access?: Array<string | undefined>,
	enforce_tfa?: boolean,
	admin_access?: boolean,
	app_access?: boolean,
	users?: Array<GraphQLTypes["update_directus_users_input"] | undefined>
};
	["update_views_translations_input"]: {
		id?: string,
	views_id?: GraphQLTypes["update_views_input"],
	languages_id?: GraphQLTypes["update_languages_input"],
	foo?: string
};
	["update_languages_input"]: {
		code?: string,
	name?: string
};
	["delete_many"]: {
	__typename: "delete_many",
	ids?: Array<string>
};
	["delete_one"]: {
	__typename: "delete_one",
	id: string
}
    }

export class GraphQLError extends Error {
    constructor(public response: GraphQLResponse) {
      super("");
      console.error(response);
    }
    toString() {
      return "GraphQL Response Error";
    }
  }


export type UnwrapPromise<T> = T extends Promise<infer R> ? R : T;
export type ZeusState<T extends (...args: any[]) => Promise<any>> = NonNullable<
  UnwrapPromise<ReturnType<T>>
>;
export type ZeusHook<
  T extends (
    ...args: any[]
  ) => Record<string, (...args: any[]) => Promise<any>>,
  N extends keyof ReturnType<T>
> = ZeusState<ReturnType<T>[N]>;

type WithTypeNameValue<T> = T & {
  __typename?: boolean;
};
type AliasType<T> = WithTypeNameValue<T> & {
  __alias?: Record<string, WithTypeNameValue<T>>;
};
export interface GraphQLResponse {
  data?: Record<string, any>;
  errors?: Array<{
    message: string;
  }>;
}
type DeepAnify<T> = {
  [P in keyof T]?: any;
};
type IsPayLoad<T> = T extends [any, infer PayLoad] ? PayLoad : T;
type IsArray<T, U> = T extends Array<infer R> ? InputType<R, U>[] : InputType<T, U>;
type FlattenArray<T> = T extends Array<infer R> ? R : T;

type IsInterfaced<SRC extends DeepAnify<DST>, DST> = FlattenArray<SRC> extends ZEUS_INTERFACES | ZEUS_UNIONS
  ? {
      [P in keyof SRC]: SRC[P] extends '__union' & infer R
        ? P extends keyof DST
          ? IsArray<R, '__typename' extends keyof DST ? DST[P] & { __typename: true } : DST[P]>
          : {}
        : never;
    }[keyof DST] &
      {
        [P in keyof Omit<
          Pick<
            SRC,
            {
              [P in keyof DST]: SRC[P] extends '__union' & infer R ? never : P;
            }[keyof DST]
          >,
          '__typename'
        >]: IsPayLoad<DST[P]> extends boolean ? SRC[P] : IsArray<SRC[P], DST[P]>;
      }
  : {
      [P in keyof Pick<SRC, keyof DST>]: IsPayLoad<DST[P]> extends boolean ? SRC[P] : IsArray<SRC[P], DST[P]>;
    };

export type MapType<SRC, DST> = SRC extends DeepAnify<DST> ? IsInterfaced<SRC, DST> : never;
export type InputType<SRC, DST> = IsPayLoad<DST> extends { __alias: infer R }
  ? {
      [P in keyof R]: MapType<SRC, R[P]>;
    } &
      MapType<SRC, Omit<IsPayLoad<DST>, '__alias'>>
  : MapType<SRC, IsPayLoad<DST>>;
type Func<P extends any[], R> = (...args: P) => R;
type AnyFunc = Func<any, any>;
export type ArgsType<F extends AnyFunc> = F extends Func<infer P, any> ? P : never;
export type OperationOptions = {
  variables?: Record<string, any>;
  operationName?: string;
};
export type SubscriptionToGraphQL<Z, T> = {
  ws: WebSocket;
  on: (fn: (args: InputType<T, Z>) => void) => void;
  off: (fn: (e: { data?: InputType<T, Z>; code?: number; reason?: string; message?: string }) => void) => void;
  error: (fn: (e: { data?: InputType<T, Z>; errors?: string[] }) => void) => void;
  open: () => void;
};
export type SelectionFunction<V> = <T>(t: T | V) => T;
export type fetchOptions = ArgsType<typeof fetch>;
type websocketOptions = typeof WebSocket extends new (
  ...args: infer R
) => WebSocket
  ? R
  : never;
export type chainOptions =
  | [fetchOptions[0], fetchOptions[1] & {websocket?: websocketOptions}]
  | [fetchOptions[0]];
export type FetchFunction = (
  query: string,
  variables?: Record<string, any>,
) => Promise<any>;
export type SubscriptionFunction = (query: string) => any;
type NotUndefined<T> = T extends undefined ? never : T;
export type ResolverType<F> = NotUndefined<F extends [infer ARGS, any] ? ARGS : undefined>;



export const ZeusSelect = <T>() => ((t: any) => t) as SelectionFunction<T>;

export const ScalarResolver = (scalar: string, value: any) => {
  switch (scalar) {
    case 'String':
      return  `${JSON.stringify(value)}`;
    case 'Int':
      return `${value}`;
    case 'Float':
      return `${value}`;
    case 'Boolean':
      return `${value}`;
    case 'ID':
      return `"${value}"`;
    case 'enum':
      return `${value}`;
    case 'scalar':
      return `${value}`;
    default:
      return false;
  }
};


export const TypesPropsResolver = ({
    value,
    type,
    name,
    key,
    blockArrays
}: {
    value: any;
    type: string;
    name: string;
    key?: string;
    blockArrays?: boolean;
}): string => {
    if (value === null) {
        return `null`;
    }
    let resolvedValue = AllTypesProps[type][name];
    if (key) {
        resolvedValue = resolvedValue[key];
    }
    if (!resolvedValue) {
        throw new Error(`Cannot resolve ${type} ${name}${key ? ` ${key}` : ''}`)
    }
    const typeResolved = resolvedValue.type;
    const isArray = resolvedValue.array;
    const isArrayRequired = resolvedValue.arrayRequired;
    if (typeof value === 'string' && value.startsWith(`ZEUS_VAR$`)) {
        const isRequired = resolvedValue.required ? '!' : '';
        let t = `${typeResolved}`;
        if (isArray) {
          if (isRequired) {
              t = `${t}!`;
          }
          t = `[${t}]`;
          if(isArrayRequired){
            t = `${t}!`;
          }
        }else{
          if (isRequired) {
                t = `${t}!`;
          }
        }
        return `\$${value.split(`ZEUS_VAR$`)[1]}__ZEUS_VAR__${t}`;
    }
    if (isArray && !blockArrays) {
        return `[${value
        .map((v: any) => TypesPropsResolver({ value: v, type, name, key, blockArrays: true }))
        .join(',')}]`;
    }
    const reslovedScalar = ScalarResolver(typeResolved, value);
    if (!reslovedScalar) {
        const resolvedType = AllTypesProps[typeResolved];
        if (typeof resolvedType === 'object') {
        const argsKeys = Object.keys(resolvedType);
        return `{${argsKeys
            .filter((ak) => value[ak] !== undefined)
            .map(
            (ak) => `${ak}:${TypesPropsResolver({ value: value[ak], type: typeResolved, name: ak })}`
            )}}`;
        }
        return ScalarResolver(AllTypesProps[typeResolved], value) as string;
    }
    return reslovedScalar;
};


const isArrayFunction = (
  parent: string[],
  a: any[]
) => {
  const [values, r] = a;
  const [mainKey, key, ...keys] = parent;
  const keyValues = Object.keys(values).filter((k) => typeof values[k] !== 'undefined');

  if (!keys.length) {
      return keyValues.length > 0
        ? `(${keyValues
            .map(
              (v) =>
                `${v}:${TypesPropsResolver({
                  value: values[v],
                  type: mainKey,
                  name: key,
                  key: v
                })}`
            )
            .join(',')})${r ? traverseToSeekArrays(parent, r) : ''}`
        : traverseToSeekArrays(parent, r);
    }

  const [typeResolverKey] = keys.splice(keys.length - 1, 1);
  let valueToResolve = ReturnTypes[mainKey][key];
  for (const k of keys) {
    valueToResolve = ReturnTypes[valueToResolve][k];
  }

  const argumentString =
    keyValues.length > 0
      ? `(${keyValues
          .map(
            (v) =>
              `${v}:${TypesPropsResolver({
                value: values[v],
                type: valueToResolve,
                name: typeResolverKey,
                key: v
              })}`
          )
          .join(',')})${r ? traverseToSeekArrays(parent, r) : ''}`
      : traverseToSeekArrays(parent, r);
  return argumentString;
};


const resolveKV = (k: string, v: boolean | string | { [x: string]: boolean | string }) =>
  typeof v === 'boolean' ? k : typeof v === 'object' ? `${k}{${objectToTree(v)}}` : `${k}${v}`;


const objectToTree = (o: { [x: string]: boolean | string }): string =>
  `{${Object.keys(o).map((k) => `${resolveKV(k, o[k])}`).join(' ')}}`;


const traverseToSeekArrays = (parent: string[], a?: any): string => {
  if (!a) return '';
  if (Object.keys(a).length === 0) {
    return '';
  }
  let b: Record<string, any> = {};
  if (Array.isArray(a)) {
    return isArrayFunction([...parent], a);
  } else {
    if (typeof a === 'object') {
      Object.keys(a)
        .filter((k) => typeof a[k] !== 'undefined')
        .forEach((k) => {
        if (k === '__alias') {
          Object.keys(a[k]).forEach((aliasKey) => {
            const aliasOperations = a[k][aliasKey];
            const aliasOperationName = Object.keys(aliasOperations)[0];
            const aliasOperation = aliasOperations[aliasOperationName];
            b[
              `${aliasOperationName}__alias__${aliasKey}: ${aliasOperationName}`
            ] = traverseToSeekArrays([...parent, aliasOperationName], aliasOperation);
          });
        } else {
          b[k] = traverseToSeekArrays([...parent, k], a[k]);
        }
      });
    } else {
      return '';
    }
  }
  return objectToTree(b);
};  


const buildQuery = (type: string, a?: Record<any, any>) => 
  traverseToSeekArrays([type], a);


const inspectVariables = (query: string) => {
  const regex = /\$\b\w*__ZEUS_VAR__\[?[^!^\]^\s^,^\)^\}]*[!]?[\]]?[!]?/g;
  let result;
  const AllVariables: string[] = [];
  while ((result = regex.exec(query))) {
    if (AllVariables.includes(result[0])) {
      continue;
    }
    AllVariables.push(result[0]);
  }
  if (!AllVariables.length) {
    return query;
  }
  let filteredQuery = query;
  AllVariables.forEach((variable) => {
    while (filteredQuery.includes(variable)) {
      filteredQuery = filteredQuery.replace(variable, variable.split('__ZEUS_VAR__')[0]);
    }
  });
  return `(${AllVariables.map((a) => a.split('__ZEUS_VAR__'))
    .map(([variableName, variableType]) => `${variableName}:${variableType}`)
    .join(', ')})${filteredQuery}`;
};


export const queryConstruct = (t: 'query' | 'mutation' | 'subscription', tName: string, operationName?: string) => (o: Record<any, any>) =>
  `${t.toLowerCase()}${operationName ? ' ' + operationName : ''}${inspectVariables(buildQuery(tName, o))}`;
  

export const fullChainConstruct = (fn: FetchFunction) => (t: 'query' | 'mutation' | 'subscription', tName: string) => (
  o: Record<any, any>,
  options?: OperationOptions,
) => fn(queryConstruct(t, tName, options?.operationName)(o), options?.variables).then((r:any) => { 
  seekForAliases(r)
  return r
});


export const fullSubscriptionConstruct = (fn: SubscriptionFunction) => (
  t: 'query' | 'mutation' | 'subscription',
  tName: string,
) => (o: Record<any, any>, options?: OperationOptions) =>
  fn(queryConstruct(t, tName, options?.operationName)(o));


const seekForAliases = (response: any) => {
  const traverseAlias = (value: any) => {
    if (Array.isArray(value)) {
      value.forEach(seekForAliases);
    } else {
      if (typeof value === 'object') {
        seekForAliases(value);
      }
    }
  };
  if (typeof response === 'object' && response) {
    const keys = Object.keys(response);
    if (keys.length < 1) {
      return;
    }
    keys.forEach((k) => {
      const value = response[k];
      if (k.indexOf('__alias__') !== -1) {
        const [operation, alias] = k.split('__alias__');
        response[alias] = {
          [operation]: value,
        };
        delete response[k];
      }
      traverseAlias(value);
    });
  }
};


export const $ = (t: TemplateStringsArray): any => `ZEUS_VAR$${t.join('')}`;


export const resolverFor = <
  X,
  T extends keyof ValueTypes,
  Z extends keyof ValueTypes[T],
>(
  type: T,
  field: Z,
  fn: (
    args: Required<ValueTypes[T]>[Z] extends [infer Input, any] ? Input : any,
    source: any,
  ) => Z extends keyof ModelTypes[T] ? ModelTypes[T][Z] | Promise<ModelTypes[T][Z]> | X : any,
) => fn as (args?: any,source?: any) => any;


const handleFetchResponse = (
  response: Parameters<Extract<Parameters<ReturnType<typeof fetch>['then']>[0], Function>>[0]
): Promise<GraphQLResponse> => {
  if (!response.ok) {
    return new Promise((_, reject) => {
      response.text().then(text => {
        try { reject(JSON.parse(text)); }
        catch (err) { reject(text); }
      }).catch(reject);
    });
  }
  return response.json();
};

export const apiFetch = (options: fetchOptions) => (query: string, variables: Record<string, any> = {}) => {
    let fetchFunction = fetch;
    let queryString = query;
    let fetchOptions = options[1] || {};
    if (fetchOptions.method && fetchOptions.method === 'GET') {
      queryString = encodeURIComponent(query);
      return fetchFunction(`${options[0]}?query=${queryString}`, fetchOptions)
        .then(handleFetchResponse)
        .then((response: GraphQLResponse) => {
          if (response.errors) {
            throw new GraphQLError(response);
          }
          return response.data;
        });
    }
    return fetchFunction(`${options[0]}`, {
      body: JSON.stringify({ query: queryString, variables }),
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      ...fetchOptions
    })
      .then(handleFetchResponse)
      .then((response: GraphQLResponse) => {
        if (response.errors) {
          throw new GraphQLError(response);
        }
        return response.data;
      });
  };
  

export const apiSubscription = (options: chainOptions) => (
    query: string,
  ) => {
    try {
      const queryString = options[0] + '?query=' + encodeURIComponent(query);
      const wsString = queryString.replace('http', 'ws');
      const host = (options.length > 1 && options[1]?.websocket?.[0]) || wsString;
      const webSocketOptions = options[1]?.websocket || [host];
      const ws = new WebSocket(...webSocketOptions);
      return {
        ws,
        on: (e: (args: any) => void) => {
          ws.onmessage = (event:any) => {
            if(event.data){
              const parsed = JSON.parse(event.data)
              const data = parsed.data
              if (data) {
                seekForAliases(data);
              }
              return e(data);
            }
          };
        },
        off: (e: (args: any) => void) => {
          ws.onclose = e;
        },
        error: (e: (args: any) => void) => {
          ws.onerror = e;
        },
        open: (e: () => void) => {
          ws.onopen = e;
        },
      };
    } catch {
      throw new Error('No websockets implemented');
    }
  };



const allOperations = {
    "query": "Query",
    "mutation": "Mutation"
}

export type GenericOperation<O> = O extends 'query'
  ? "Query"
  : O extends 'mutation'
  ? "Mutation"
  : never

export const Thunder = (fn: FetchFunction) => <
  O extends 'query' | 'mutation',
  R extends keyof ValueTypes = GenericOperation<O>
>(
  operation: O,
) => <Z extends ValueTypes[R]>(o: Z | ValueTypes[R], ops?: OperationOptions) =>
  fullChainConstruct(fn)(operation, allOperations[operation])(o as any, ops) as Promise<InputType<GraphQLTypes[R], Z>>;

export const Chain = (...options: chainOptions) => Thunder(apiFetch(options));  
  
export const SubscriptionThunder = (fn: SubscriptionFunction) => <
  O extends 'query' | 'mutation',
  R extends keyof ValueTypes = GenericOperation<O>
>(
  operation: O,
) => <Z extends ValueTypes[R]>(
  o: Z | ValueTypes[R],
  ops?: OperationOptions
)=>
  fullSubscriptionConstruct(fn)(operation, allOperations[operation])(
    o as any,
    ops,
  ) as SubscriptionToGraphQL<Z, GraphQLTypes[R]>;

export const Subscription = (...options: chainOptions) => SubscriptionThunder(apiSubscription(options));
export const Zeus = <
  Z extends ValueTypes[R],
  O extends 'query' | 'mutation',
  R extends keyof ValueTypes = GenericOperation<O>
>(
  operation: O,
  o: Z | ValueTypes[R],
  operationName?: string,
) => queryConstruct(operation, allOperations[operation], operationName)(o as any);
export const Selector = <T extends keyof ValueTypes>(key: T) => ZeusSelect<ValueTypes[T]>();
  