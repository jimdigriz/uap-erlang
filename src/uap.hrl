-record(uap_ua, {
	family		:: string(),
	major		:: string(),
	minor		:: string(),
	patch		:: string()
}).

-type uap_ua() :: #uap_ua{}.

-record(uap_os, {
	os		:: string(),
	major		:: string(),
	minor		:: string(),
	patch		:: string(),
	patch_minor	:: string()
}).

-type uap_os() :: #uap_os{}.

-record(uap_device, {
	family		:: string(),
	brand		:: string(),
	model		:: string()
}).

-type uap_device() :: #uap_device{}.
