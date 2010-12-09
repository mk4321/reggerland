{ Data types and constants

  @Author  Alexey Yarochkin
  @Version 27.01.2005 v0.1
}

unit EggerData;

interface

const
  MapCellSize = 32;
  GridCellSize = 16;
  GridsInMap = MapCellSize div GridCellSize;
  ShiftMapToGrid = 1;
  PauseTimeMax = 127;
  RecoveryTimeMax = 255;

  MapWidth = 15;
  MapInfoWidth = 5;
  MapHeight = 15;
  LevelMapWidth = 10;
  LevelMapHeight = 10;
  minLevelMapFloor = -1;
  maxLevelMapFloor = 3;
  ScreenWidth = 640;
  ScreenHeight = 480;

type
  TTileType = (tNothing,
    tGrass, tRoad, tIce, tGranite, tSand, tWater, tLava,
    tStone, tTree,
    tBridgeV, tBridgeH,
    tGold, tDiamond, tGoldInf, tBoxer, tEraser, tChest, tKey, tMasterkey, tRaft,
    tSpanner, tHourglass, tSoporific, tMagnet, tHammer,
    tBorderTL, tBorderT, tBorderTR,
    tBorderL, tBorderR,
    tBorderBL, tBorderB, tBorderBR,
    tDoorIO, tDoorTO, tDoorLO, tDoorRO, tDoorBO, tDoorDO, tDoorUO,
    tDoorT, tDoorL, tDoorR, tDoorB, tDoorD, tDoorU,
    tArrowU, tArrowL, tArrowR, tArrowD,
    tLive, tBullet,
    tEmpty = $FF);

  TTiles = set of TTileType;

const
  CollectedItems = [tBridgeV..tBridgeH, tGold..tMasterKey];
  HiddenItems = [tBridgeV, tSpanner..tHammer];
  ChestItems = [tKey..tRaft];
  ArrowItems = [tArrowU..tArrowD];
  FragileItems = [tStone..tTree];
  LiquidItems = [tWater..tLava];
  SpriteHolders = [tEmpty, tDoorDO, tDoorUO, tDoorD, tDoorU];
  BoxStepBacks = [tGrass..tSand];
  HeroStepBacks = BoxStepBacks + [tNothing, tEmpty];
  AnyoneStepBacks = BoxStepBacks - [tRoad];
  BulletStepBacks = BoxStepBacks + LiquidItems;
  GhostStepBacks = BulletStepBacks;
  AnyoneStepItems = [tBridgeV, tRaft..tHammer, tDoorD, tDoorU,
    tArrowU..tArrowD, tEmpty];
  HeroStepItems = [tBridgeV..tHammer, tDoorIO..tDoorUO, tDoorD, tDoorU,
    tArrowU..tArrowD, tEmpty];
  GhostStepItems = [tStone..tHammer, tDoorDO, tDoorUO, tDoorD, tDoorU,
    tArrowU..tArrowD, tEmpty];
  BulletStepItems = [tTree..tBridgeH, tKey..tHammer, tDoorD, tDoorU,
    tArrowU..tArrowD, tEmpty];

type
  TGameState = (
    gsStartLevel,  // persistent, not moving
    gsPlayLevel,   // persistent, moving
    gsOpenChest,   // transient
    gsCatchKey,    // persistent, moving
    gsOpenDoors,   // transient
    gsNextLevel,   // transient
    gsKillHero,    // transient
    gsQuit);       // transient

const
  PersistentStates = [gsStartLevel, gsPlayLevel, gsCatchKey];
  MovingStates = [gsPlayLevel, gsCatchKey];

type
  TSpriteState = (ssNormal, ssPaused, ssRecovering, ssSwimming, ssSleeping);

const
  EggStates = [ssPaused, ssSwimming];

type
  TSpriteType = (
    sHero,     {00}
    sBox,      {10}
    sDragon,   {20}
    sSkull,    {30}
    sRoller,   {40}
    sFireball, {50}
    sSleepy,   {60}
    sMedusa,   {70}
    sStony,    {80}
    sCuckold,  {90}
    sBullet,   {xx}
    sFire,     {xx}
    sKnife,    {xx}
    sFlash,    {xx}
    sEgg,      {xx}
    sRaft      {xx});

const
  Ammunition = [sBullet, sFire, sKnife, sFlash];

  SSpriteFile: array [TSpriteType] of string = (
    'Hero0.png', 'Box.png', 'Dragon.png', 'Skull.png', 'Roller.png',
    'Fireball.png', 'Sleepy.png', 'Medusa.png', 'Stony.png', 'Cuckold.png',
    'Bullet.png', 'Fire.png', 'Knife.png', 'Flash.png', 'Egg.png', 'Raft.png');

type
  TSoundType = (
    sndShoot, sndRecharge, sndGold, sndUnhide, sndKey, sndTrick, sndKill);

const
  SSoundFile: array [TSoundType] of string = ('shoot.wav', 'recharge.wav',
    'gold.wav', 'unhide.wav', 'key.wav', 'trick.wav', 'kill.wav');

type
  TMapLayer = (mlBack, mlMid, mlItem);

  TMapCell = record
    case Byte of
      0: (
        Back, Mid, Item: TTileType;
        Sprite: Byte);
      1: (
        Z: array [TMapLayer] of TTileType;
        Weight: Byte);
  end;

  TDelta = -1..1;
  TSpeed = 0..MapCellSize;
  TLevelFlags = array [minLevelMapFloor..maxLevelMapFloor,
                       0..LevelMapHeight - 1,
                       0..LevelMapWidth - 1] of Boolean;

  T3DCoord = record
    x, y, z: ShortInt;
  end;
  TGameRecord = record
    LevelDone: TLevelFlags;
    Level: T3DCoord;
    x, y: SmallInt;
    dx, dy: TDelta;
  end;

resourcestring
  SAppTitle = 'Egger';
  SInit = 'INIT';
  SFinal = 'FINI';
  SSDLInitError = 'SDL initialization failed';
  SAudioInitError = 'SDL mixer initialization failed';
  SSoundLoadError = 'Error loading sound: ';
  SBackgroundError = 'Creating background failed';
  SVideoModeError = 'Setting video mode failed';
  SVideoMode = 'Selected video mode: ';
  SImageError = 'Error loading image: ';
  SMapOpenError = 'Map file not found';
  SMapReadError = 'Error reading map file';
  SIniReadError = 'Error reading ini file';

  SGFXDir = 'gfx\';
  SMapDir = 'maps\';
  SSndDir = 'snd\';
  SIniFile = 'maps.ini';
  SGenSection = 'General';
  SStart = 'Start';
  SMapFile = 'maps.dat';
  SGameStateFile = 'Egger.sav';
  SIconFile = 'Box.png';
  STilesFile = 'Tiles.png';
  SFontFile = 'Font.png';

const
  ItoDX: array [0..3] of TDelta = (0, -1, 1, 0);
  ItoDY: array [0..3] of TDelta = (-1, 0, 0, 1);

function RectsIntersect(
  aLeft1, aRight1, aTop1, aBottom1,
  aLeft2, aRight2, aTop2, aBottom2: SmallInt): Boolean;
function PixelToGrid(aValue: SmallInt): SmallInt; register;
function PixelToMap(aValue: SmallInt): SmallInt; register;
function DeltaToIndex(dX, dY: TDelta): Byte; register;
function ifop(aCondition: Boolean; aTrueResult, aFalseResult: SmallInt): SmallInt; register; overload;
function ifop(aCondition: Boolean; aTrueResult, aFalseResult: TTileType): TTileType; register; overload;
function ifop(aCondition: Boolean; aTrueResult, aFalseResult: TSpriteState): TSpriteState; register; overload;
function sgn(aValue: SmallInt): TDelta; register;

implementation

function RectsIntersect(
  aLeft1, aRight1, aTop1, aBottom1,
  aLeft2, aRight2, aTop2, aBottom2: SmallInt): Boolean;
begin
  Result := (aLeft1 <= aRight2) and (aLeft2 <= aRight1)
    and (aTop1 <= aBottom2) and (aTop2 <= aBottom1);
end;

function PixelToGrid(aValue: SmallInt): SmallInt;
begin
  Result := aValue div GridCellSize;
end;

function PixelToMap(aValue: SmallInt): SmallInt;
begin
  Result := aValue div MapCellSize;
end;

{ . 0 .
  1 . 2
  . 3 . }

function DeltaToIndex(dx, dy: TDelta): Byte;
const
  cdX: array[TDelta] of Byte = (1, 0, 2);
  cdY: array[TDelta] of Byte = (0, 0, 3);
begin
  Result := cdX[dX] + cdY[dY];
end;

function ifop(aCondition: Boolean; aTrueResult, aFalseResult: SmallInt): SmallInt;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function ifop(aCondition: Boolean; aTrueResult, aFalseResult: TTileType): TTileType;
begin
  Result := TTileType(ifop(aCondition, Ord(aTrueResult), Ord(aFalseResult)));
end;

function ifop(aCondition: Boolean; aTrueResult, aFalseResult: TSpriteState): TSpriteState;
begin
  Result := TSpriteState(ifop(aCondition, Ord(aTrueResult), Ord(aFalseResult)));
end;

function sgn(aValue: SmallInt): TDelta;
begin
  Result := ifop(aValue > 0, 1, ifop(aValue < 0, -1, 0));
end;

end.

