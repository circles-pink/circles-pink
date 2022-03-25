/* eslint-disable */

export const AllTypesProps: Record<string,any> = {
	Query:{
		names:{
			filter:{
				type:"names_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		},
		names_by_id:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		names_aggregated:{
			groupBy:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			filter:{
				type:"names_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			}
		},
		views:{
			filter:{
				type:"views_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		},
		views_by_id:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		views_aggregated:{
			groupBy:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			filter:{
				type:"views_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			}
		},
		views_translations:{
			filter:{
				type:"views_translations_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		},
		views_translations_by_id:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		views_translations_aggregated:{
			groupBy:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			filter:{
				type:"views_translations_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			}
		},
		languages:{
			filter:{
				type:"languages_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		},
		languages_by_id:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		}
	},
	names_filter:{
		id:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		_and:{
			type:"names_filter",
			array:true,
			arrayRequired:false,
			required:false
		},
		_or:{
			type:"names_filter",
			array:true,
			arrayRequired:false,
			required:false
		}
	},
	number_filter_operators:{
		_eq:{
			type:"Float",
			array:false,
			arrayRequired:false,
			required:false
		},
		_neq:{
			type:"Float",
			array:false,
			arrayRequired:false,
			required:false
		},
		_in:{
			type:"Float",
			array:true,
			arrayRequired:false,
			required:false
		},
		_nin:{
			type:"Float",
			array:true,
			arrayRequired:false,
			required:false
		},
		_gt:{
			type:"Float",
			array:false,
			arrayRequired:false,
			required:false
		},
		_gte:{
			type:"Float",
			array:false,
			arrayRequired:false,
			required:false
		},
		_lt:{
			type:"Float",
			array:false,
			arrayRequired:false,
			required:false
		},
		_lte:{
			type:"Float",
			array:false,
			arrayRequired:false,
			required:false
		},
		_null:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		},
		_nnull:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	string_filter_operators:{
		_eq:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_neq:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_contains:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_ncontains:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_starts_with:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_nstarts_with:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_ends_with:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_nends_with:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_in:{
			type:"String",
			array:true,
			arrayRequired:false,
			required:false
		},
		_nin:{
			type:"String",
			array:true,
			arrayRequired:false,
			required:false
		},
		_null:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		},
		_nnull:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		},
		_empty:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		},
		_nempty:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	JSON: "String",
	views:{
		name:{
			filter:{
				type:"names_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		},
		translations:{
			filter:{
				type:"views_translations_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		}
	},
	Date: "String",
	views_translations:{
		views_id:{
			filter:{
				type:"views_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		},
		languages_id:{
			filter:{
				type:"languages_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			}
		}
	},
	views_filter:{
		user_created:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		user_updated:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		id:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		status:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		sort:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_created:{
			type:"date_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_created_func:{
			type:"datetime_function_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_updated:{
			type:"date_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_updated_func:{
			type:"datetime_function_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		enum:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"names_filter",
			array:false,
			arrayRequired:false,
			required:false
		},
		translations:{
			type:"views_translations_filter",
			array:false,
			arrayRequired:false,
			required:false
		},
		_and:{
			type:"views_filter",
			array:true,
			arrayRequired:false,
			required:false
		},
		_or:{
			type:"views_filter",
			array:true,
			arrayRequired:false,
			required:false
		}
	},
	date_filter_operators:{
		_eq:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_neq:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_gt:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_gte:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_lt:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_lte:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		_null:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		},
		_nnull:{
			type:"Boolean",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	datetime_function_filter_operators:{
		year:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		month:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		week:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		day:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		weekday:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		hour:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		minute:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		second:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	views_translations_filter:{
		id:{
			type:"number_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		views_id:{
			type:"views_filter",
			array:false,
			arrayRequired:false,
			required:false
		},
		languages_id:{
			type:"languages_filter",
			array:false,
			arrayRequired:false,
			required:false
		},
		foo:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		_and:{
			type:"views_translations_filter",
			array:true,
			arrayRequired:false,
			required:false
		},
		_or:{
			type:"views_translations_filter",
			array:true,
			arrayRequired:false,
			required:false
		}
	},
	languages_filter:{
		code:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"string_filter_operators",
			array:false,
			arrayRequired:false,
			required:false
		},
		_and:{
			type:"languages_filter",
			array:true,
			arrayRequired:false,
			required:false
		},
		_or:{
			type:"languages_filter",
			array:true,
			arrayRequired:false,
			required:false
		}
	},
	Mutation:{
		create_names_items:{
			filter:{
				type:"names_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			data:{
				type:"create_names_input",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		create_names_item:{
			data:{
				type:"create_names_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		create_views_items:{
			filter:{
				type:"views_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			data:{
				type:"create_views_input",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		create_views_item:{
			data:{
				type:"create_views_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		create_views_translations_items:{
			filter:{
				type:"views_translations_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			data:{
				type:"create_views_translations_input",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		create_views_translations_item:{
			data:{
				type:"create_views_translations_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		create_languages_items:{
			filter:{
				type:"languages_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			data:{
				type:"create_languages_input",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		create_languages_item:{
			data:{
				type:"create_languages_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_names_items:{
			filter:{
				type:"names_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_names_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_names_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_names_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_views_items:{
			filter:{
				type:"views_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_views_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_views_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_views_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_views_translations_items:{
			filter:{
				type:"views_translations_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_views_translations_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_views_translations_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_views_translations_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_languages_items:{
			filter:{
				type:"languages_filter",
				array:false,
				arrayRequired:false,
				required:false
			},
			sort:{
				type:"String",
				array:true,
				arrayRequired:false,
				required:false
			},
			limit:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			offset:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			page:{
				type:"Int",
				array:false,
				arrayRequired:false,
				required:false
			},
			search:{
				type:"String",
				array:false,
				arrayRequired:false,
				required:false
			},
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_languages_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		update_languages_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			},
			data:{
				type:"update_languages_input",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		delete_names_items:{
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		delete_names_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		delete_views_items:{
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		delete_views_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		delete_views_translations_items:{
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		delete_views_translations_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		},
		delete_languages_items:{
			ids:{
				type:"ID",
				array:true,
				arrayRequired:false,
				required:true
			}
		},
		delete_languages_item:{
			id:{
				type:"ID",
				array:false,
				arrayRequired:false,
				required:true
			}
		}
	},
	create_names_input:{
		id:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:true
		}
	},
	create_views_input:{
		user_created:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		user_updated:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		id:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		status:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:true
		},
		sort:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_created:{
			type:"Date",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_created_func:{
			type:"datetime_functionsInput",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_updated:{
			type:"Date",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_updated_func:{
			type:"datetime_functionsInput",
			array:false,
			arrayRequired:false,
			required:false
		},
		enum:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:true
		},
		name:{
			type:"create_names_input",
			array:false,
			arrayRequired:false,
			required:false
		},
		translations:{
			type:"create_views_translations_input",
			array:true,
			arrayRequired:false,
			required:false
		}
	},
	datetime_functionsInput:{
		year:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		month:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		week:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		day:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		weekday:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		hour:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		minute:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		second:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	create_views_translations_input:{
		id:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		views_id:{
			type:"create_views_input",
			array:false,
			arrayRequired:false,
			required:false
		},
		languages_id:{
			type:"create_languages_input",
			array:false,
			arrayRequired:false,
			required:false
		},
		foo:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:true
		}
	},
	create_languages_input:{
		code:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	update_names_input:{
		id:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	update_views_input:{
		user_created:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		user_updated:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		id:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		status:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		sort:{
			type:"Int",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_created:{
			type:"Date",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_created_func:{
			type:"datetime_functionsInput",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_updated:{
			type:"Date",
			array:false,
			arrayRequired:false,
			required:false
		},
		date_updated_func:{
			type:"datetime_functionsInput",
			array:false,
			arrayRequired:false,
			required:false
		},
		enum:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"update_names_input",
			array:false,
			arrayRequired:false,
			required:false
		},
		translations:{
			type:"update_views_translations_input",
			array:true,
			arrayRequired:false,
			required:false
		}
	},
	update_views_translations_input:{
		id:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		views_id:{
			type:"update_views_input",
			array:false,
			arrayRequired:false,
			required:false
		},
		languages_id:{
			type:"update_languages_input",
			array:false,
			arrayRequired:false,
			required:false
		},
		foo:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		}
	},
	update_languages_input:{
		code:{
			type:"ID",
			array:false,
			arrayRequired:false,
			required:false
		},
		name:{
			type:"String",
			array:false,
			arrayRequired:false,
			required:false
		}
	}
}

export const ReturnTypes: Record<string,any> = {
	Query:{
		names:"names",
		names_by_id:"names",
		names_aggregated:"names_aggregated",
		views:"views",
		views_by_id:"views",
		views_aggregated:"views_aggregated",
		views_translations:"views_translations",
		views_translations_by_id:"views_translations",
		views_translations_aggregated:"views_translations_aggregated",
		languages:"languages",
		languages_by_id:"languages"
	},
	names:{
		id:"ID",
		name:"String"
	},
	names_aggregated:{
		group:"JSON",
		avg:"names_aggregated_fields",
		sum:"names_aggregated_fields",
		count:"names_aggregated_fields",
		countDistinct:"names_aggregated_fields",
		avgDistinct:"names_aggregated_fields",
		sumDistinct:"names_aggregated_fields",
		min:"names_aggregated_fields",
		max:"names_aggregated_fields"
	},
	names_aggregated_fields:{
		id:"Float"
	},
	views:{
		user_created:"String",
		user_updated:"String",
		id:"ID",
		status:"String",
		sort:"Int",
		date_created:"Date",
		date_created_func:"datetime_functions",
		date_updated:"Date",
		date_updated_func:"datetime_functions",
		enum:"String",
		name:"names",
		translations:"views_translations"
	},
	datetime_functions:{
		year:"Int",
		month:"Int",
		week:"Int",
		day:"Int",
		weekday:"Int",
		hour:"Int",
		minute:"Int",
		second:"Int"
	},
	views_translations:{
		id:"ID",
		views_id:"views",
		languages_id:"languages",
		foo:"String"
	},
	languages:{
		code:"ID",
		name:"String"
	},
	views_aggregated:{
		group:"JSON",
		avg:"views_aggregated_fields",
		sum:"views_aggregated_fields",
		count:"views_aggregated_fields",
		countDistinct:"views_aggregated_fields",
		avgDistinct:"views_aggregated_fields",
		sumDistinct:"views_aggregated_fields",
		min:"views_aggregated_fields",
		max:"views_aggregated_fields"
	},
	views_aggregated_fields:{
		sort:"Float",
		name:"Float"
	},
	views_translations_aggregated:{
		group:"JSON",
		avg:"views_translations_aggregated_fields",
		sum:"views_translations_aggregated_fields",
		count:"views_translations_aggregated_fields",
		countDistinct:"views_translations_aggregated_fields",
		avgDistinct:"views_translations_aggregated_fields",
		sumDistinct:"views_translations_aggregated_fields",
		min:"views_translations_aggregated_fields",
		max:"views_translations_aggregated_fields"
	},
	views_translations_aggregated_fields:{
		id:"Float"
	},
	Mutation:{
		create_names_items:"names",
		create_names_item:"names",
		create_views_items:"views",
		create_views_item:"views",
		create_views_translations_items:"views_translations",
		create_views_translations_item:"views_translations",
		create_languages_items:"languages",
		create_languages_item:"languages",
		update_names_items:"names",
		update_names_item:"names",
		update_views_items:"views",
		update_views_item:"views",
		update_views_translations_items:"views_translations",
		update_views_translations_item:"views_translations",
		update_languages_items:"languages",
		update_languages_item:"languages",
		delete_names_items:"delete_many",
		delete_names_item:"delete_one",
		delete_views_items:"delete_many",
		delete_views_item:"delete_one",
		delete_views_translations_items:"delete_many",
		delete_views_translations_item:"delete_one",
		delete_languages_items:"delete_many",
		delete_languages_item:"delete_one"
	},
	delete_many:{
		ids:"ID"
	},
	delete_one:{
		id:"ID"
	}
}