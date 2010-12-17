{ Data types and constants

  @Author  Alexey Yarochkin
  @Version 07.12.2010 v0.2
  @History 27.01.2005 v0.1
}

unit EggerData;

interface

type
  TPixCoord = SmallInt;
  TMapCoord = SmallInt;
  TGridCoord = SmallInt;
  TLevelTimer = SmallInt;
  TDistance = SmallInt;

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
    { terrain }
    tGrass, tRoad, tIce, tBasalt, tSand, tWater, tLava,
    { decor }
    tStone, tTree,
    { bridge }
    tBridgeVertical, tBridgeHorizontal,
    { item }
    tHeart, tDiamond, tHeartInf, tBoxer, tEraser, tChest, tKey, tMasterKey, tRaft,
    { tool }
    tSpanner, tHourglass, tHypnotic, tMagnet, tHammer,
    { barrier }
    tBorderTopLeft, tBorderTop, tBorderTopRight, tBorderLeft, tBorderRight,
    tBorderBottomLeft, tBorderBottom, tBorderBottomRight,
    { door }
    tDoorOpenInside, tDoorOpenTop, tDoorOpenLeft,
    tDoorOpenRight, tDoorOpenBottom, tDoorOpenDown, tDoorOpenUp,
    tDoorTop, tDoorLeft, tDoorRight, tDoorBottom, tDoorDown, tDoorUp,
    { directed road }
    tArrowUp, tArrowLeft, tArrowRight, tArrowDown,
    tLive, tBullet,
    tEmpty = $FF);

  TTiles = set of TTileType;

const
  CollectedItems = [tBridgeVertical..tBridgeHorizontal, tHeart..tMasterKey];
  HiddenItems = [tBridgeVertical, tSpanner..tHammer];
  ChestItems = [tKey..tRaft];
  ArrowItems = [tArrowUp..tArrowDown];
  FragileItems = [tStone..tTree];
  LiquidItems = [tWater..tLava];
  SpriteHolders = [tEmpty, tDoorOpenDown, tDoorOpenUp, tDoorDown, tDoorUp];
  BoxPathBacks = [tGrass..tSand];
  HeroPathBacks = BoxPathBacks + [tNothing, tEmpty];
  AnyPathBacks = BoxPathBacks - [tRoad];
  BulletPathBacks = BoxPathBacks + LiquidItems;
  GhostPathBacks = BulletPathBacks;
  AnyPathItems = [tBridgeVertical, tRaft..tHammer, tDoorDown, tDoorUp,
    tArrowUp..tArrowDown, tEmpty];
  HeroPathItems = [tBridgeVertical..tHammer, tDoorOpenInside..tDoorOpenUp,
    tDoorDown, tDoorUp, tArrowUp..tArrowDown, tEmpty];
  GhostPathItems = [tStone..tHammer, tDoorOpenDown, tDoorOpenUp, tDoorDown, tDoorUp,
    tArrowUp..tArrowDown, tEmpty];
  BulletPathItems = [tTree..tBridgeHorizontal, tKey..tHammer, tDoorDown, tDoorUp,
    tArrowUp..tArrowDown, tEmpty];

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
  GameStateNames: array[TGameState] of String = (
    'StartLevel', 'PlayLevel', 'OpenChest', 'CatchKey',
    'OpenDoors', 'NextLevel', 'KillHero', 'Quit');

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
    sBullet,   {  }
    sFire,     {  }
    sKnife,    {  }
    sFlash,    {  }
    sEgg,      {  }
    sRaft      {  });

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
  TLevelStatus = (lsNew, lsDone);
  TMapLevelStatus = array [minLevelMapFloor..maxLevelMapFloor,
                          0..LevelMapHeight - 1,
                          0..LevelMapWidth - 1] of TLevelStatus;

  T3DMapCoord = record
    mapX, mapY, mapZ: TMapCoord;
  end;
  TGameRecord = record
    MapLevelStatus: TMapLevelStatus;
    Level: T3DMapCoord;
    X, Y: TPixCoord;
    dX, dY: TDelta;
  end;

const
  LevelStatusNames: array[TLevelStatus] of String = ('NEW', 'DONE');

resourcestring
  SAppTitle = 'Egger';
  SSDLInitError = 'SDL initialization failed';
  SAudioInitError = 'SDL mixer initialization failed';
  SBackgroundError = 'Creating background failed';
  SVideoModeError = 'Setting video mode failed';
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
  aLeft2, aRight2, aTop2, aBottom2: TPixCoord): Boolean;
function PixToGrid(aValue: TPixCoord): TGridCoord; register;
function GridToPix(aValue: TGridCoord): TPixCoord; register;
function PixToMap(aValue: TPixCoord): TMapCoord; register;
function MapToPix(aValue: TMapCoord): TPixCoord; register;
function GridToMap(aValue: TGridCoord): TMapCoord; register;
function MapToGrid(aValue: TMapCoord): TGridCoord; register;
function DeltaToIndex(adX, adY: TDelta): Byte; register;
function OffsetTileType(aTileType: TTileType; adX, adY: TDelta;
  aReverse: Boolean = False): TTileType;
function ifop(aCondition: Boolean; aTrueResult, aFalseResult: SmallInt): SmallInt; register; overload;
function ifop(aCondition: Boolean; aTrueResult, aFalseResult: TTileType): TTileType; register; overload;
function ifop(aCondition: Boolean; aTrueResult, aFalseResult: TSpriteState): TSpriteState; register; overload;
function sgn(aValue: SmallInt): TDelta; register;

implementation

function RectsIntersect(
  aLeft1, aRight1, aTop1, aBottom1,
  aLeft2, aRight2, aTop2, aBottom2: TPixCoord): Boolean;
begin
  Result := (aLeft1 <= aRight2) and (aLeft2 <= aRight1)
    and (aTop1 <= aBottom2) and (aTop2 <= aBottom1);
end;

function PixToGrid(aValue: TPixCoord): TGridCoord;
begin
  Result := aValue div GridCellSize;
end;

function GridToPix(aValue: TGridCoord): TPixCoord;
begin
  Result := aValue * GridCellSize;
end;

function PixToMap(aValue: TPixCoord): TMapCoord;
begin
  Result := aValue div MapCellSize;
end;

function MapToPix(aValue: TMapCoord): TPixCoord;
begin
  Result := aValue * MapCellSize;
end;

{ . 0 .
  1 . 2
  . 3 . }

function GridToMap(aValue: TGridCoord): TMapCoord; register;
begin
  Result := aValue shr ShiftMapToGrid;
end;

function MapToGrid(aValue: TMapCoord): TGridCoord; register;
begin
  Result := aValue shl ShiftMapToGrid;
end;

function DeltaToIndex(adx, ady: TDelta): Byte;
const
  cdX: array[TDelta] of Byte = (1, 0, 2);
  cdY: array[TDelta] of Byte = (0, 0, 3);
begin
  Result := cdX[adX] + cdY[adY];
end;

function OffsetTileType(aTileType: TTileType; adX, adY: TDelta;
  aReverse: Boolean): TTileType;
var
  Index: Byte;
begin
  Index := DeltaToIndex(adX, adY);
  if aReverse then
    Index := -Index;
  Result := TTileType(Ord(aTileType) + Index);
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

