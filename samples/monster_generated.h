// automatically generated by the FlatBuffers compiler, do not modify


#ifndef FLATBUFFERS_GENERATED_MONSTER_MYGAME_SAMPLE_H_
#define FLATBUFFERS_GENERATED_MONSTER_MYGAME_SAMPLE_H_

#include "flatbuffers/flatbuffers.h"

namespace MyGame {
namespace Sample {

struct Vec3T;

struct MonsterT;
struct Monster;

struct WeaponT;
struct Weapon;

bool operator==(const Vec3T &lhs, const Vec3T &rhs);
bool operator==(const Monster &lhs, const Monster &rhs);
bool operator==(const Weapon &lhs, const Weapon &rhs);

inline const flatbuffers::TypeTable *Vec3TypeTable();

inline const flatbuffers::TypeTable *MonsterTypeTable();

inline const flatbuffers::TypeTable *WeaponTypeTable();

enum Color {
  Color_Red = 0,
  Color_Green = 1,
  Color_Blue = 2,
  Color_MIN = Color_Red,
  Color_MAX = Color_Blue
};

inline const Color (&EnumValuesColor())[3] {
  static const Color values[] = {
    Color_Red,
    Color_Green,
    Color_Blue
  };
  return values;
}

inline const char * const *EnumNamesColor() {
  static const char * const names[] = {
    "Red",
    "Green",
    "Blue",
    nullptr
  };
  return names;
}

inline const char *EnumNameColor(Color e) {
  if (e < Color_Red || e > Color_Blue) return "";
  const size_t index = static_cast<int>(e);
  return EnumNamesColor()[index];
}

enum Equipment {
  Equipment_NONE = 0,
  Equipment_Weapon = 1,
  Equipment_MIN = Equipment_NONE,
  Equipment_MAX = Equipment_Weapon
};

inline const Equipment (&EnumValuesEquipment())[2] {
  static const Equipment values[] = {
    Equipment_NONE,
    Equipment_Weapon
  };
  return values;
}

inline const char * const *EnumNamesEquipment() {
  static const char * const names[] = {
    "NONE",
    "Weapon",
    nullptr
  };
  return names;
}

inline const char *EnumNameEquipment(Equipment e) {
  if (e < Equipment_NONE || e > Equipment_Weapon) return "";
  const size_t index = static_cast<int>(e);
  return EnumNamesEquipment()[index];
}

template<typename T> struct EquipmentTraits {
  static const Equipment enum_value = Equipment_NONE;
};

template<> struct EquipmentTraits<WeaponT> {
  static const Equipment enum_value = Equipment_Weapon;
};

struct EquipmentUnion {
  Equipment type;
  void *value;

  EquipmentUnion() : type(Equipment_NONE), value(nullptr) {}
  EquipmentUnion(EquipmentUnion&& u) FLATBUFFERS_NOEXCEPT :
    type(Equipment_NONE), value(nullptr)
    { std::swap(type, u.type); std::swap(value, u.value); }
  EquipmentUnion(const EquipmentUnion &) FLATBUFFERS_NOEXCEPT;
  EquipmentUnion &operator=(const EquipmentUnion &u) FLATBUFFERS_NOEXCEPT
    { EquipmentUnion t(u); std::swap(type, t.type); std::swap(value, t.value); return *this; }
  EquipmentUnion &operator=(EquipmentUnion &&u) FLATBUFFERS_NOEXCEPT
    { std::swap(type, u.type); std::swap(value, u.value); return *this; }
  ~EquipmentUnion() { Reset(); }

  void Reset();

#ifndef FLATBUFFERS_CPP98_STL
  template <typename T>
  void Set(T&& val) {
    Reset();
    type = EquipmentTraits<typename T::TableType>::enum_value;
    if (type != Equipment_NONE) {
      value = new T(std::forward<T>(val));
    }
  }
#endif  // FLATBUFFERS_CPP98_STL

  static void *UnPack(const void *obj, Equipment type, const flatbuffers::resolver_function_t *resolver);
  flatbuffers::Offset<void> Pack(flatbuffers::FlatBufferBuilder &_fbb, const flatbuffers::rehasher_function_t *_rehasher = nullptr) const;

  Weapon *AsWeapon() {
    return type == Equipment_Weapon ?
      reinterpret_cast<Weapon *>(value) : nullptr;
  }
  const Weapon *AsWeapon() const {
    return type == Equipment_Weapon ?
      reinterpret_cast<const Weapon *>(value) : nullptr;
  }
};


inline bool operator==(const EquipmentUnion &lhs, const EquipmentUnion &rhs) {
  if (lhs.type != rhs.type) return false;
  switch (lhs.type) {
    case Equipment_NONE: {
      return true;
    }
    case Equipment_Weapon: {
      return *(reinterpret_cast<const Weapon *>(lhs.value)) ==
             *(reinterpret_cast<const Weapon *>(rhs.value));
    }
    default: {
      return false;
    }
  }
}
bool VerifyEquipment(flatbuffers::Verifier &verifier, const void *obj, Equipment type);
bool VerifyEquipmentVector(flatbuffers::Verifier &verifier, const flatbuffers::Vector<flatbuffers::Offset<void>> *values, const flatbuffers::Vector<uint8_t> *types);

FLATBUFFERS_MANUALLY_ALIGNED_STRUCT(4) Vec3T FLATBUFFERS_FINAL_CLASS {
 private:
  float x_;
  float y_;
  float z_;

 public:
  Vec3T() {
    memset(static_cast<void *>(this), 0, sizeof(Vec3T));
  }
  Vec3T(float _x, float _y, float _z)
      : x_(flatbuffers::EndianScalar(_x)),
        y_(flatbuffers::EndianScalar(_y)),
        z_(flatbuffers::EndianScalar(_z)) {
  }
  float x() const {
    return flatbuffers::EndianScalar(x_);
  }
  void mutate_x(float _x) {
    flatbuffers::WriteScalar(&x_, _x);
  }
  float y() const {
    return flatbuffers::EndianScalar(y_);
  }
  void mutate_y(float _y) {
    flatbuffers::WriteScalar(&y_, _y);
  }
  float z() const {
    return flatbuffers::EndianScalar(z_);
  }
  void mutate_z(float _z) {
    flatbuffers::WriteScalar(&z_, _z);
  }
};
FLATBUFFERS_STRUCT_END(Vec3T, 12);

inline bool operator==(const Vec3T &lhs, const Vec3T &rhs) {
  return
      (lhs.x() == rhs.x()) &&
      (lhs.y() == rhs.y()) &&
      (lhs.z() == rhs.z());
}

struct Monster : public flatbuffers::NativeTable {
  typedef MonsterT TableType;
  flatbuffers::unique_ptr<Vec3T> pos;
  int16_t mana;
  int16_t hp;
  std::string name;
  std::vector<uint8_t> inventory;
  Color color;
  std::vector<flatbuffers::unique_ptr<Weapon>> weapons;
  EquipmentUnion equipped;
  Monster()
      : mana(150),
        hp(100),
        color(Color_Blue) {
  }
};

inline bool operator==(const Monster &lhs, const Monster &rhs) {
  return
      (lhs.pos == rhs.pos) &&
      (lhs.mana == rhs.mana) &&
      (lhs.hp == rhs.hp) &&
      (lhs.name == rhs.name) &&
      (lhs.inventory == rhs.inventory) &&
      (lhs.color == rhs.color) &&
      (lhs.weapons == rhs.weapons) &&
      (lhs.equipped == rhs.equipped);
}

struct MonsterT FLATBUFFERS_FINAL_CLASS : private flatbuffers::Table {
  typedef Monster NativeTableType;
  static const flatbuffers::TypeTable *MiniReflectTypeTable() {
    return MonsterTypeTable();
  }
  enum FlatBuffersVTableOffset FLATBUFFERS_VTABLE_UNDERLYING_TYPE {
    VT_POS = 4,
    VT_MANA = 6,
    VT_HP = 8,
    VT_NAME = 10,
    VT_INVENTORY = 14,
    VT_COLOR = 16,
    VT_WEAPONS = 18,
    VT_EQUIPPED_TYPE = 20,
    VT_EQUIPPED = 22
  };
  const Vec3T *pos() const {
    return GetStruct<const Vec3T *>(VT_POS);
  }
  Vec3T *mutable_pos() {
    return GetStruct<Vec3T *>(VT_POS);
  }
  int16_t mana() const {
    return GetField<int16_t>(VT_MANA, 150);
  }
  bool mutate_mana(int16_t _mana) {
    return SetField<int16_t>(VT_MANA, _mana, 150);
  }
  int16_t hp() const {
    return GetField<int16_t>(VT_HP, 100);
  }
  bool mutate_hp(int16_t _hp) {
    return SetField<int16_t>(VT_HP, _hp, 100);
  }
  const flatbuffers::String *name() const {
    return GetPointer<const flatbuffers::String *>(VT_NAME);
  }
  flatbuffers::String *mutable_name() {
    return GetPointer<flatbuffers::String *>(VT_NAME);
  }
  const flatbuffers::Vector<uint8_t> *inventory() const {
    return GetPointer<const flatbuffers::Vector<uint8_t> *>(VT_INVENTORY);
  }
  flatbuffers::Vector<uint8_t> *mutable_inventory() {
    return GetPointer<flatbuffers::Vector<uint8_t> *>(VT_INVENTORY);
  }
  Color color() const {
    return static_cast<Color>(GetField<int8_t>(VT_COLOR, 2));
  }
  bool mutate_color(Color _color) {
    return SetField<int8_t>(VT_COLOR, static_cast<int8_t>(_color), 2);
  }
  const flatbuffers::Vector<flatbuffers::Offset<WeaponT>> *weapons() const {
    return GetPointer<const flatbuffers::Vector<flatbuffers::Offset<WeaponT>> *>(VT_WEAPONS);
  }
  flatbuffers::Vector<flatbuffers::Offset<WeaponT>> *mutable_weapons() {
    return GetPointer<flatbuffers::Vector<flatbuffers::Offset<WeaponT>> *>(VT_WEAPONS);
  }
  Equipment equipped_type() const {
    return static_cast<Equipment>(GetField<uint8_t>(VT_EQUIPPED_TYPE, 0));
  }
  bool mutate_equipped_type(Equipment _equipped_type) {
    return SetField<uint8_t>(VT_EQUIPPED_TYPE, static_cast<uint8_t>(_equipped_type), 0);
  }
  const void *equipped() const {
    return GetPointer<const void *>(VT_EQUIPPED);
  }
  template<typename T> const T *equipped_as() const;
  const WeaponT *equipped_as_Weapon() const {
    return equipped_type() == Equipment_Weapon ? static_cast<const WeaponT *>(equipped()) : nullptr;
  }
  void *mutable_equipped() {
    return GetPointer<void *>(VT_EQUIPPED);
  }
  bool Verify(flatbuffers::Verifier &verifier) const {
    return VerifyTableStart(verifier) &&
           VerifyField<Vec3T>(verifier, VT_POS) &&
           VerifyField<int16_t>(verifier, VT_MANA) &&
           VerifyField<int16_t>(verifier, VT_HP) &&
           VerifyOffset(verifier, VT_NAME) &&
           verifier.VerifyString(name()) &&
           VerifyOffset(verifier, VT_INVENTORY) &&
           verifier.VerifyVector(inventory()) &&
           VerifyField<int8_t>(verifier, VT_COLOR) &&
           VerifyOffset(verifier, VT_WEAPONS) &&
           verifier.VerifyVector(weapons()) &&
           verifier.VerifyVectorOfTables(weapons()) &&
           VerifyField<uint8_t>(verifier, VT_EQUIPPED_TYPE) &&
           VerifyOffset(verifier, VT_EQUIPPED) &&
           VerifyEquipment(verifier, equipped(), equipped_type()) &&
           verifier.EndTable();
  }
  Monster *UnPack(const flatbuffers::resolver_function_t *_resolver = nullptr) const;
  void UnPackTo(Monster *_o, const flatbuffers::resolver_function_t *_resolver = nullptr) const;
  static void UnPackToFrom(Monster *_o, const MonsterT *_fb, const flatbuffers::resolver_function_t *_resolver = nullptr);
  static flatbuffers::Offset<MonsterT> Pack(flatbuffers::FlatBufferBuilder &_fbb, const Monster* _o, const flatbuffers::rehasher_function_t *_rehasher = nullptr);
};

template<> inline const WeaponT *MonsterT::equipped_as<WeaponT>() const {
  return equipped_as_Weapon();
}

struct MonsterBuilder {
  flatbuffers::FlatBufferBuilder &fbb_;
  flatbuffers::uoffset_t start_;
  void add_pos(const Vec3T *pos) {
    fbb_.AddStruct(MonsterT::VT_POS, pos);
  }
  void add_mana(int16_t mana) {
    fbb_.AddElement<int16_t>(MonsterT::VT_MANA, mana, 150);
  }
  void add_hp(int16_t hp) {
    fbb_.AddElement<int16_t>(MonsterT::VT_HP, hp, 100);
  }
  void add_name(flatbuffers::Offset<flatbuffers::String> name) {
    fbb_.AddOffset(MonsterT::VT_NAME, name);
  }
  void add_inventory(flatbuffers::Offset<flatbuffers::Vector<uint8_t>> inventory) {
    fbb_.AddOffset(MonsterT::VT_INVENTORY, inventory);
  }
  void add_color(Color color) {
    fbb_.AddElement<int8_t>(MonsterT::VT_COLOR, static_cast<int8_t>(color), 2);
  }
  void add_weapons(flatbuffers::Offset<flatbuffers::Vector<flatbuffers::Offset<WeaponT>>> weapons) {
    fbb_.AddOffset(MonsterT::VT_WEAPONS, weapons);
  }
  void add_equipped_type(Equipment equipped_type) {
    fbb_.AddElement<uint8_t>(MonsterT::VT_EQUIPPED_TYPE, static_cast<uint8_t>(equipped_type), 0);
  }
  void add_equipped(flatbuffers::Offset<void> equipped) {
    fbb_.AddOffset(MonsterT::VT_EQUIPPED, equipped);
  }
  explicit MonsterBuilder(flatbuffers::FlatBufferBuilder &_fbb)
        : fbb_(_fbb) {
    start_ = fbb_.StartTable();
  }
  MonsterBuilder &operator=(const MonsterBuilder &);
  flatbuffers::Offset<MonsterT> Finish() {
    const auto end = fbb_.EndTable(start_);
    auto o = flatbuffers::Offset<MonsterT>(end);
    return o;
  }
};

inline flatbuffers::Offset<MonsterT> CreateMonster(
    flatbuffers::FlatBufferBuilder &_fbb,
    const Vec3T *pos = 0,
    int16_t mana = 150,
    int16_t hp = 100,
    flatbuffers::Offset<flatbuffers::String> name = 0,
    flatbuffers::Offset<flatbuffers::Vector<uint8_t>> inventory = 0,
    Color color = Color_Blue,
    flatbuffers::Offset<flatbuffers::Vector<flatbuffers::Offset<WeaponT>>> weapons = 0,
    Equipment equipped_type = Equipment_NONE,
    flatbuffers::Offset<void> equipped = 0) {
  MonsterBuilder builder_(_fbb);
  builder_.add_equipped(equipped);
  builder_.add_weapons(weapons);
  builder_.add_inventory(inventory);
  builder_.add_name(name);
  builder_.add_pos(pos);
  builder_.add_hp(hp);
  builder_.add_mana(mana);
  builder_.add_equipped_type(equipped_type);
  builder_.add_color(color);
  return builder_.Finish();
}

inline flatbuffers::Offset<MonsterT> CreateMonsterDirect(
    flatbuffers::FlatBufferBuilder &_fbb,
    const Vec3T *pos = 0,
    int16_t mana = 150,
    int16_t hp = 100,
    const char *name = nullptr,
    const std::vector<uint8_t> *inventory = nullptr,
    Color color = Color_Blue,
    const std::vector<flatbuffers::Offset<WeaponT>> *weapons = nullptr,
    Equipment equipped_type = Equipment_NONE,
    flatbuffers::Offset<void> equipped = 0) {
  auto name__ = name ? _fbb.CreateString(name) : 0;
  auto inventory__ = inventory ? _fbb.CreateVector<uint8_t>(*inventory) : 0;
  auto weapons__ = weapons ? _fbb.CreateVector<flatbuffers::Offset<WeaponT>>(*weapons) : 0;
  return MyGame::Sample::CreateMonster(
      _fbb,
      pos,
      mana,
      hp,
      name__,
      inventory__,
      color,
      weapons__,
      equipped_type,
      equipped);
}

flatbuffers::Offset<MonsterT> CreateMonster(flatbuffers::FlatBufferBuilder &_fbb, const Monster *_o, const flatbuffers::rehasher_function_t *_rehasher = nullptr);

struct Weapon : public flatbuffers::NativeTable {
  typedef WeaponT TableType;
  std::string name;
  int16_t damage;
  Weapon()
      : damage(0) {
  }
};

inline bool operator==(const Weapon &lhs, const Weapon &rhs) {
  return
      (lhs.name == rhs.name) &&
      (lhs.damage == rhs.damage);
}

struct WeaponT FLATBUFFERS_FINAL_CLASS : private flatbuffers::Table {
  typedef Weapon NativeTableType;
  static const flatbuffers::TypeTable *MiniReflectTypeTable() {
    return WeaponTypeTable();
  }
  enum FlatBuffersVTableOffset FLATBUFFERS_VTABLE_UNDERLYING_TYPE {
    VT_NAME = 4,
    VT_DAMAGE = 6
  };
  const flatbuffers::String *name() const {
    return GetPointer<const flatbuffers::String *>(VT_NAME);
  }
  flatbuffers::String *mutable_name() {
    return GetPointer<flatbuffers::String *>(VT_NAME);
  }
  int16_t damage() const {
    return GetField<int16_t>(VT_DAMAGE, 0);
  }
  bool mutate_damage(int16_t _damage) {
    return SetField<int16_t>(VT_DAMAGE, _damage, 0);
  }
  bool Verify(flatbuffers::Verifier &verifier) const {
    return VerifyTableStart(verifier) &&
           VerifyOffset(verifier, VT_NAME) &&
           verifier.VerifyString(name()) &&
           VerifyField<int16_t>(verifier, VT_DAMAGE) &&
           verifier.EndTable();
  }
  Weapon *UnPack(const flatbuffers::resolver_function_t *_resolver = nullptr) const;
  void UnPackTo(Weapon *_o, const flatbuffers::resolver_function_t *_resolver = nullptr) const;
  static void UnPackToFrom(Weapon *_o, const WeaponT *_fb, const flatbuffers::resolver_function_t *_resolver = nullptr);
  static flatbuffers::Offset<WeaponT> Pack(flatbuffers::FlatBufferBuilder &_fbb, const Weapon* _o, const flatbuffers::rehasher_function_t *_rehasher = nullptr);
};

struct WeaponBuilder {
  flatbuffers::FlatBufferBuilder &fbb_;
  flatbuffers::uoffset_t start_;
  void add_name(flatbuffers::Offset<flatbuffers::String> name) {
    fbb_.AddOffset(WeaponT::VT_NAME, name);
  }
  void add_damage(int16_t damage) {
    fbb_.AddElement<int16_t>(WeaponT::VT_DAMAGE, damage, 0);
  }
  explicit WeaponBuilder(flatbuffers::FlatBufferBuilder &_fbb)
        : fbb_(_fbb) {
    start_ = fbb_.StartTable();
  }
  WeaponBuilder &operator=(const WeaponBuilder &);
  flatbuffers::Offset<WeaponT> Finish() {
    const auto end = fbb_.EndTable(start_);
    auto o = flatbuffers::Offset<WeaponT>(end);
    return o;
  }
};

inline flatbuffers::Offset<WeaponT> CreateWeapon(
    flatbuffers::FlatBufferBuilder &_fbb,
    flatbuffers::Offset<flatbuffers::String> name = 0,
    int16_t damage = 0) {
  WeaponBuilder builder_(_fbb);
  builder_.add_name(name);
  builder_.add_damage(damage);
  return builder_.Finish();
}

inline flatbuffers::Offset<WeaponT> CreateWeaponDirect(
    flatbuffers::FlatBufferBuilder &_fbb,
    const char *name = nullptr,
    int16_t damage = 0) {
  auto name__ = name ? _fbb.CreateString(name) : 0;
  return MyGame::Sample::CreateWeapon(
      _fbb,
      name__,
      damage);
}

flatbuffers::Offset<WeaponT> CreateWeapon(flatbuffers::FlatBufferBuilder &_fbb, const Weapon *_o, const flatbuffers::rehasher_function_t *_rehasher = nullptr);

inline Monster *MonsterT::UnPack(const flatbuffers::resolver_function_t *_resolver) const {
  auto _o = new Monster();
  UnPackTo(_o, _resolver);
  return _o;
}

inline void MonsterT::UnPackTo(Monster *_o, const flatbuffers::resolver_function_t *_resolver) const {
  (void)_o;
  (void)_resolver;
  UnPackToFrom(_o, this, _resolver);
}

inline void MonsterT::UnPackToFrom(Monster *_o, const MonsterT *_fb, const flatbuffers::resolver_function_t *_resolver) {
  (void)_o;
  (void)_fb;
  (void)_resolver;
  { auto _e = _fb->pos(); if (_e) _o->pos = flatbuffers::unique_ptr<Vec3T>(new Vec3T(*_e)); };
  { auto _e = _fb->mana(); _o->mana = _e; };
  { auto _e = _fb->hp(); _o->hp = _e; };
  { auto _e = _fb->name(); if (_e) _o->name = _e->str(); };
  { auto _e = _fb->inventory(); if (_e) { _o->inventory.resize(_e->size()); for (flatbuffers::uoffset_t _i = 0; _i < _e->size(); _i++) { _o->inventory[_i] = _e->Get(_i); } } };
  { auto _e = _fb->color(); _o->color = _e; };
  { auto _e = _fb->weapons(); if (_e) { _o->weapons.resize(_e->size()); for (flatbuffers::uoffset_t _i = 0; _i < _e->size(); _i++) { _o->weapons[_i] = flatbuffers::unique_ptr<Weapon>(_e->Get(_i)->UnPack(_resolver)); } } };
  { auto _e = _fb->equipped_type(); _o->equipped.type = _e; };
  { auto _e = _fb->equipped(); if (_e) _o->equipped.value = EquipmentUnion::UnPack(_e, _fb->equipped_type(), _resolver); };
}

inline flatbuffers::Offset<MonsterT> MonsterT::Pack(flatbuffers::FlatBufferBuilder &_fbb, const Monster* _o, const flatbuffers::rehasher_function_t *_rehasher) {
  return CreateMonster(_fbb, _o, _rehasher);
}

inline flatbuffers::Offset<MonsterT> CreateMonster(flatbuffers::FlatBufferBuilder &_fbb, const Monster *_o, const flatbuffers::rehasher_function_t *_rehasher) {
  (void)_rehasher;
  (void)_o;
  struct _VectorArgs { flatbuffers::FlatBufferBuilder *__fbb; const Monster* __o; const flatbuffers::rehasher_function_t *__rehasher; } _va = { &_fbb, _o, _rehasher}; (void)_va;
  auto _pos = _o->pos ? _o->pos.get() : 0;
  auto _mana = _o->mana;
  auto _hp = _o->hp;
  auto _name = _o->name.empty() ? 0 : _fbb.CreateString(_o->name);
  auto _inventory = _o->inventory.size() ? _fbb.CreateVector(_o->inventory) : 0;
  auto _color = _o->color;
  auto _weapons = _o->weapons.size() ? _fbb.CreateVector<flatbuffers::Offset<WeaponT>> (_o->weapons.size(), [](size_t i, _VectorArgs *__va) { return CreateWeapon(*__va->__fbb, __va->__o->weapons[i].get(), __va->__rehasher); }, &_va ) : 0;
  auto _equipped_type = _o->equipped.type;
  auto _equipped = _o->equipped.Pack(_fbb);
  return MyGame::Sample::CreateMonster(
      _fbb,
      _pos,
      _mana,
      _hp,
      _name,
      _inventory,
      _color,
      _weapons,
      _equipped_type,
      _equipped);
}

inline Weapon *WeaponT::UnPack(const flatbuffers::resolver_function_t *_resolver) const {
  auto _o = new Weapon();
  UnPackTo(_o, _resolver);
  return _o;
}

inline void WeaponT::UnPackTo(Weapon *_o, const flatbuffers::resolver_function_t *_resolver) const {
  (void)_o;
  (void)_resolver;
  UnPackToFrom(_o, this, _resolver);
}

inline void WeaponT::UnPackToFrom(Weapon *_o, const WeaponT *_fb, const flatbuffers::resolver_function_t *_resolver) {
  (void)_o;
  (void)_fb;
  (void)_resolver;
  { auto _e = _fb->name(); if (_e) _o->name = _e->str(); };
  { auto _e = _fb->damage(); _o->damage = _e; };
}

inline flatbuffers::Offset<WeaponT> WeaponT::Pack(flatbuffers::FlatBufferBuilder &_fbb, const Weapon* _o, const flatbuffers::rehasher_function_t *_rehasher) {
  return CreateWeapon(_fbb, _o, _rehasher);
}

inline flatbuffers::Offset<WeaponT> CreateWeapon(flatbuffers::FlatBufferBuilder &_fbb, const Weapon *_o, const flatbuffers::rehasher_function_t *_rehasher) {
  (void)_rehasher;
  (void)_o;
  struct _VectorArgs { flatbuffers::FlatBufferBuilder *__fbb; const Weapon* __o; const flatbuffers::rehasher_function_t *__rehasher; } _va = { &_fbb, _o, _rehasher}; (void)_va;
  auto _name = _o->name.empty() ? 0 : _fbb.CreateString(_o->name);
  auto _damage = _o->damage;
  return MyGame::Sample::CreateWeapon(
      _fbb,
      _name,
      _damage);
}

inline bool VerifyEquipment(flatbuffers::Verifier &verifier, const void *obj, Equipment type) {
  switch (type) {
    case Equipment_NONE: {
      return true;
    }
    case Equipment_Weapon: {
      auto ptr = reinterpret_cast<const WeaponT *>(obj);
      return verifier.VerifyTable(ptr);
    }
    default: return false;
  }
}

inline bool VerifyEquipmentVector(flatbuffers::Verifier &verifier, const flatbuffers::Vector<flatbuffers::Offset<void>> *values, const flatbuffers::Vector<uint8_t> *types) {
  if (!values || !types) return !values && !types;
  if (values->size() != types->size()) return false;
  for (flatbuffers::uoffset_t i = 0; i < values->size(); ++i) {
    if (!VerifyEquipment(
        verifier,  values->Get(i), types->GetEnum<Equipment>(i))) {
      return false;
    }
  }
  return true;
}

inline void *EquipmentUnion::UnPack(const void *obj, Equipment type, const flatbuffers::resolver_function_t *resolver) {
  switch (type) {
    case Equipment_Weapon: {
      auto ptr = reinterpret_cast<const WeaponT *>(obj);
      return ptr->UnPack(resolver);
    }
    default: return nullptr;
  }
}

inline flatbuffers::Offset<void> EquipmentUnion::Pack(flatbuffers::FlatBufferBuilder &_fbb, const flatbuffers::rehasher_function_t *_rehasher) const {
  switch (type) {
    case Equipment_Weapon: {
      auto ptr = reinterpret_cast<const Weapon *>(value);
      return CreateWeapon(_fbb, ptr, _rehasher).Union();
    }
    default: return 0;
  }
}

inline EquipmentUnion::EquipmentUnion(const EquipmentUnion &u) FLATBUFFERS_NOEXCEPT : type(u.type), value(nullptr) {
  switch (type) {
    case Equipment_Weapon: {
      value = new Weapon(*reinterpret_cast<Weapon *>(u.value));
      break;
    }
    default:
      break;
  }
}

inline void EquipmentUnion::Reset() {
  switch (type) {
    case Equipment_Weapon: {
      auto ptr = reinterpret_cast<Weapon *>(value);
      delete ptr;
      break;
    }
    default: break;
  }
  value = nullptr;
  type = Equipment_NONE;
}

inline const flatbuffers::TypeTable *ColorTypeTable() {
  static const flatbuffers::TypeCode type_codes[] = {
    { flatbuffers::ET_CHAR, 0, 0 },
    { flatbuffers::ET_CHAR, 0, 0 },
    { flatbuffers::ET_CHAR, 0, 0 }
  };
  static const flatbuffers::TypeFunction type_refs[] = {
    ColorTypeTable
  };
  static const char * const names[] = {
    "Red",
    "Green",
    "Blue"
  };
  static const flatbuffers::TypeTable tt = {
    flatbuffers::ST_ENUM, 3, type_codes, type_refs, nullptr, names
  };
  return &tt;
}

inline const flatbuffers::TypeTable *EquipmentTypeTable() {
  static const flatbuffers::TypeCode type_codes[] = {
    { flatbuffers::ET_SEQUENCE, 0, -1 },
    { flatbuffers::ET_SEQUENCE, 0, 0 }
  };
  static const flatbuffers::TypeFunction type_refs[] = {
    WeaponTypeTable
  };
  static const char * const names[] = {
    "NONE",
    "Weapon"
  };
  static const flatbuffers::TypeTable tt = {
    flatbuffers::ST_UNION, 2, type_codes, type_refs, nullptr, names
  };
  return &tt;
}

inline const flatbuffers::TypeTable *Vec3TypeTable() {
  static const flatbuffers::TypeCode type_codes[] = {
    { flatbuffers::ET_FLOAT, 0, -1 },
    { flatbuffers::ET_FLOAT, 0, -1 },
    { flatbuffers::ET_FLOAT, 0, -1 }
  };
  static const int64_t values[] = { 0, 4, 8, 12 };
  static const char * const names[] = {
    "x",
    "y",
    "z"
  };
  static const flatbuffers::TypeTable tt = {
    flatbuffers::ST_STRUCT, 3, type_codes, nullptr, values, names
  };
  return &tt;
}

inline const flatbuffers::TypeTable *MonsterTypeTable() {
  static const flatbuffers::TypeCode type_codes[] = {
    { flatbuffers::ET_SEQUENCE, 0, 0 },
    { flatbuffers::ET_SHORT, 0, -1 },
    { flatbuffers::ET_SHORT, 0, -1 },
    { flatbuffers::ET_STRING, 0, -1 },
    { flatbuffers::ET_BOOL, 0, -1 },
    { flatbuffers::ET_UCHAR, 1, -1 },
    { flatbuffers::ET_CHAR, 0, 1 },
    { flatbuffers::ET_SEQUENCE, 1, 2 },
    { flatbuffers::ET_UTYPE, 0, 3 },
    { flatbuffers::ET_SEQUENCE, 0, 3 }
  };
  static const flatbuffers::TypeFunction type_refs[] = {
    Vec3TypeTable,
    ColorTypeTable,
    WeaponTypeTable,
    EquipmentTypeTable
  };
  static const char * const names[] = {
    "pos",
    "mana",
    "hp",
    "name",
    "friendly",
    "inventory",
    "color",
    "weapons",
    "equipped_type",
    "equipped"
  };
  static const flatbuffers::TypeTable tt = {
    flatbuffers::ST_TABLE, 10, type_codes, type_refs, nullptr, names
  };
  return &tt;
}

inline const flatbuffers::TypeTable *WeaponTypeTable() {
  static const flatbuffers::TypeCode type_codes[] = {
    { flatbuffers::ET_STRING, 0, -1 },
    { flatbuffers::ET_SHORT, 0, -1 }
  };
  static const char * const names[] = {
    "name",
    "damage"
  };
  static const flatbuffers::TypeTable tt = {
    flatbuffers::ST_TABLE, 2, type_codes, nullptr, nullptr, names
  };
  return &tt;
}

inline const MyGame::Sample::MonsterT *GetMonster(const void *buf) {
  return flatbuffers::GetRoot<MyGame::Sample::MonsterT>(buf);
}

inline const MyGame::Sample::MonsterT *GetSizePrefixedMonster(const void *buf) {
  return flatbuffers::GetSizePrefixedRoot<MyGame::Sample::MonsterT>(buf);
}

inline MonsterT *GetMutableMonster(void *buf) {
  return flatbuffers::GetMutableRoot<MonsterT>(buf);
}

inline bool VerifyMonsterBuffer(
    flatbuffers::Verifier &verifier) {
  return verifier.VerifyBuffer<MyGame::Sample::MonsterT>(nullptr);
}

inline bool VerifySizePrefixedMonsterBuffer(
    flatbuffers::Verifier &verifier) {
  return verifier.VerifySizePrefixedBuffer<MyGame::Sample::MonsterT>(nullptr);
}

inline void FinishMonsterBuffer(
    flatbuffers::FlatBufferBuilder &fbb,
    flatbuffers::Offset<MyGame::Sample::MonsterT> root) {
  fbb.Finish(root);
}

inline void FinishSizePrefixedMonsterBuffer(
    flatbuffers::FlatBufferBuilder &fbb,
    flatbuffers::Offset<MyGame::Sample::MonsterT> root) {
  fbb.FinishSizePrefixed(root);
}

inline flatbuffers::unique_ptr<Monster> UnPackMonster(
    const void *buf,
    const flatbuffers::resolver_function_t *res = nullptr) {
  return flatbuffers::unique_ptr<Monster>(GetMonster(buf)->UnPack(res));
}

}  // namespace Sample
}  // namespace MyGame

#endif  // FLATBUFFERS_GENERATED_MONSTER_MYGAME_SAMPLE_H_
