pub type FileId = u32;
pub type ItemId = u32;

pub type TypeId = u64;

pub type SID = (FileId, ItemId);
pub type SSID = (FileId, ItemId, Vec<TypeId>);
