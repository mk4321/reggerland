{ Implements game container

  @Author  Alexey Yarochkin
  @Version 11.02.2005 v0.1
}

unit EggerGame;

interface

uses
  SDL, SDLSprites, SDL_Mixer, EggerSprite, EggerData, IniFiles;

type
  TGame = class
  private
    Screen,
    Background,
    TileImage,
    FontImage: PSDL_Surface;
    ox, oy: SmallInt;
    odx, ody: TDelta;
    FIniFile: TIniFile;
    FMap: array [0..MapHeight - 1, 0..MapWidth - 1] of TMapCell;
    FInfo: array [0..MapHeight - 1, MapWidth..MapWidth + MapInfoWidth - 1] of TMapCell;
    FStartLevel: string;
    FLevelDone: TLevelFlags;
    FLevelTimer: Smallint;
    FSounds: array [TSoundType] of PMix_Chunk;
    function GetMap(mx, my: SmallInt): TMapCell;
    procedure LoadMap(var Map; Length: Word);
    function LoadImage(const Name: string): PSDL_Surface;
    procedure LoadGame;
    procedure SaveGame;
    procedure CreateSprites;
    procedure DeleteSprites(All: Boolean);
    procedure DrawMap;
    procedure OpenChest;
    procedure OpenDoors(aState: TGameState);
    procedure KillHero;
    procedure NextLevel;
    function GetLevelDone: Boolean;
    procedure SetLevelDone(Value: Boolean);
    procedure SetLevelTimer(Value: SmallInt);
    function ParseLevel(const S: string): Boolean;
  public
    Counter: Byte;
    GameState: TGameState;
    SpriteEngine: TSpriteEngine;
    SpriteImage: array [TSpriteType] of PSDL_Surface;
    Hero: THero;
    Level: T3DCoord;

    constructor Create;
    procedure Initialize;
    procedure PlayGame;
    procedure Finalize;

    procedure DrawBackground;
    procedure DrawTile(mx, my: SmallInt; Update: Boolean);
    procedure DrawInfo;
    procedure PlaySound(Sound: TSoundType);
    procedure WriteText(const Text: string; aX, aY: SmallInt);

    function TileCount(Tiles: TTiles; CountWeight: Boolean = False): Word;
    function SpriteIn(gx1, gx2, gy1, gy2: SmallInt;
      Exclude: TActiveSprite = nil): TActiveSprite;
    procedure ReplaceTiles(STile, DTile: TTileType; Layer: TMapLayer = mlItem;
      Exclude: ShortInt = -1);
    procedure CheckHidden(Unhide: Boolean = False);

    property Map[mx, my: SmallInt]: TMapCell read GetMap;
    procedure SetMap(mx, my: SmallInt; Layer: TMapLayer; Value: TTileType);
    procedure SetNextLevel(dx, dy, dz: TDelta);
    property LevelDone: Boolean read GetLevelDone write SetLevelDone;
    property LevelTimer: Smallint read FLevelTimer write SetLevelTimer;
  end;

var
  Game: TGame;

implementation

uses
  SDLUtils, SDL_Image, Logger;

procedure FatalError(const S: string);
begin
//  MessageBox(0, PChar(S), '', MB_ICONERROR + MB_OK);
  Log.LogError(S, SInit);
  Halt;
end;

{ TGame }

constructor TGame.Create;
begin
  Randomize;
  GameState := gsNextLevel;
  LoadMap(FInfo, SizeOf(FInfo));
end;

procedure TGame.Initialize;
const
  cbpp: array [0..4] of Byte = (15, 16, 24, 32, 8);
var
  i, bpp: Byte;
  st: TSpriteType;
  snd: TSoundType;
  s: string;
begin
  // initializing SDL engine
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) = -1 then
    FatalError(SSDLInitError);

  // setting window properties
  SDL_WM_SetCaption(PChar(SAppTitle), nil);
  SDL_WM_SetIcon(IMG_Load(PChar(SGFXDir + SIconFile)), 0);

  // initializing video mode
  for i := Low(cbpp) to High(cbpp) do
  begin
    bpp := SDL_VideoModeOK(ScreenWidth, ScreenHeight, cbpp[i], 0);
    if bpp <> 0 then Break;
  end;
  Str(bpp, S);
  log.LogStatus(SVideoMode + S + ' bpp', SInit);

  Screen := SDL_SetVideoMode(ScreenWidth, ScreenHeight, bpp,
    SDL_SWSURFACE or SDL_HWPALETTE {or SDL_DOUBLEBUF} {or SDL_FULLSCREEN});
  if Screen = nil then
    FatalError(SVideoModeError);

  // initializing background surface
  Background := SDL_CreateRGBSurface(
    SDL_SWSURFACE or SDL_HWPALETTE, ScreenWidth, ScreenHeight, bpp, 0, 0, 0, 0);
  if Background = nil then
    FatalError(SBackgroundError + ': ' + SDL_GetError);
  Background := SDL_DisplayFormat(Background);

  // loading images
  TileImage := LoadImage(STilesFile);
  FontImage := LoadImage(SFontFile);
  for st := Low(TSpriteType) to High(TSpriteType) do
    SpriteImage[st] := LoadImage(SSpriteFile[st]);
  SDL_ShowCursor(SDL_DISABLE);

  // initializing sprite engine
  SpriteEngine := TSpriteEngine.Create(Screen);
  SpriteEngine.BackgroundSurface := Background;

  // initializing audio
  if Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT,
    MIX_DEFAULT_CHANNELS, 2048) < 0 then
      FatalError(SAudioInitError);
//Mix_HookMusicFinished(@RestartMusic);
  for snd := Low(TSoundType) to High(TSoundType)do
  begin
    FSounds[snd] := Mix_LoadWav(PChar(SSndDir + SSoundFile[snd]));
    if FSounds[snd] = nil then
      log.LogWarning(SSoundLoadError + SSoundFile[snd], SInit);
  end;
  FIniFile := TIniFile.Create(SMapDir + SIniFile);
  FStartLevel := FIniFile.ReadString(SGenSection, SStart, '');
//  FStartLevel := 'L0806';
  // setting initial level
  if not ParseLevel(FStartLevel) then
    FatalError(SIniReadError);
  // creating Hero
  Hero := THero.Create(sHero, $7, 7 * MapCellSize, 3 * MapCellSize);
end;

type
  TKeyInfo = record
    // SDL key code
    SDLK: Word;
    // if key can be placed in stack only once while pressed
    Once: Boolean;
  end;

const
  KeyInfo: array [0..10] of TKeyInfo = (
    (SDLK: 0; Once: False),
    (SDLK: SDLK_SPACE; Once: True),
    (SDLK: SDLK_LEFT; Once: False),
    (SDLK: SDLK_RIGHT; Once: False),
    (SDLK: SDLK_UP; Once: False),
    (SDLK: SDLK_DOWN; Once: False),
    (SDLK: SDLK_F2; Once: True),
    (SDLK: SDLK_F3; Once: True),
    (SDLK: SDLK_F10; Once: True),
    (SDLK: SDLK_DELETE; Once: True),
    (SDLK: SDLK_ESCAPE; Once: True)
  );

procedure TGame.PlayGame;
const
  StartingLevel: array [Boolean] of TGameState = (gsPlayLevel, gsCatchKey);
var
  Now, NextTime: Cardinal;
  Event: TSDL_Event;
  Keys: PKeyStateArr;
  isDown: Boolean;
  si, osi: Byte;

procedure CheckKeys;
var
  i: Byte;
begin
  for i := Low(KeyInfo) to High(KeyInfo) do
    // if a pressed key is found
    if Keys[KeyInfo[i].SDLK] = 1 then
    begin
      // key can be placed in stack if
      // - it can be pressed more than once (continuously)
      // - it can be pressed once and the key was not down just before
      if not KeyInfo[i].Once or not isDown then
        if (osi <> i) or (Counter and $3 = 0) then
          si := i;
      isDown := True;
      Exit;
    end;
  // if no pressed keys are found isDown flag is reset to false
  isDown := False;
  osi := 0;
end;

procedure ProcessDebugKeys;
begin
  if (Keys[SDLK_LCTRL] + Keys[SDLK_LSHIFT] = 2) then
    case KeyInfo[si].SDLK of
      SDLK_DELETE: OpenDoors(gsPlayLevel);
    end
    else Exit;
  si := 0;
  Hero.Ghostly := True;
end;

begin
  NextTime := 0;
  si := 0;
  osi := 0;

  // game loop start
  repeat
    while SDL_PollEvent(@Event) > 0 do;

    Keys := PKeyStateArr(SDL_GetKeyState(nil));
    CheckKeys;
    // keys are processed from stack, in persistent modes only
    if (si <> 0) and (GameState in PersistentStates) then
      with Hero do
      begin
        // hero activities are processed when hero is on the grid edge
        if Counter and $3 = 0 then
        begin
          ProcessDebugKeys;
          case KeyInfo[si].SDLK of
            SDLK_SPACE:
              Attack(dx, dy);
            SDLK_LEFT:
              Pushed(-1, 0);
            SDLK_RIGHT:
              Pushed(1, 0);
            SDLK_UP:
              Pushed(0, -1);
            SDLK_DOWN:
              Pushed(0, 1);
            SDLK_ESCAPE:
              GameState := gsQuit;
            SDLK_F2:
              SaveGame;
            SDLK_F3:
              LoadGame;
            SDLK_F10:
              GameState := gsKillHero;
          end;
          osi := si;
          si := 0;
        end;
      end;

    // level starts moving if a key is pressed
    if isDown and (GameState = gsStartLevel) then
    begin
      GameState := StartingLevel[LevelDone or (TileCount([tGold]) = 0)];
      DrawBackground;
      Counter := 0;
    end;

    case GameState of
      gsOpenChest:
        OpenChest;
      gsOpenDoors:
        OpenDoors(gsPlayLevel);
      gsNextLevel:
        NextLevel;
      gsKillHero:
        KillHero;
      else if GameState in MovingStates then
      begin
        if (LevelTimer > 0) and (Counter and $1F = 0) then
        begin
          LevelTimer := LevelTimer - 1;
          if LevelTimer = 0 then
            KillHero;
        end;
        SpriteEngine.Move;
        Inc(Counter);
      end;
    end;

    // updating sprites on the screen
    SpriteEngine.Draw;
    SDL_UpdateRect(Screen, 0, 0, 0, 0);

    // remaining time delay at the end of loop
    Now := SDL_GetTicks;
    if NextTime > Now then
      SDL_Delay(NextTime - Now);
    NextTime := SDL_GetTicks + Cardinal(ifop(Keys[SDLK_LALT] = 1, 10, 40));

  until GameState = gsQuit;
  // game loop end
end;

procedure TGame.Finalize;
var
  st: TSpriteType;
  snd: TSoundType;
begin
  FIniFile.Free;
  // finalizing audio
  for snd := Low(TSoundType) to High(TSoundType)do
    if FSounds[snd] <> nil then
      Mix_FreeChunk(FSounds[snd]);
  Mix_CloseAudio;

  // finalizing sprite engine
  if SpriteEngine <> nil then
    SpriteEngine.Free;

  // finalizing images
  for st := Low(TSpriteType) to High(TSpriteType) do
    if SpriteImage[st] <> nil then
      SDL_FreeSurface(SpriteImage[st]);
  if FontImage <> nil then
    SDL_FreeSurface(FontImage);
  if TileImage <> nil then
    SDL_FreeSurface(TileImage);

  // finalizing background surface
  if Background <> nil then
    SDL_FreeSurface(Background);

  // closing video mode
  if Screen <> nil then
    SDL_FreeSurface(Screen);

  // finalizing SDL engine
  SDL_Quit;
  // destroying the game itself
  Free;
end;

function TGame.LoadImage(const Name: string): PSDL_Surface;
begin
  Result := IMG_Load(PChar(SGFXDir + Name));
  if Result = nil then
    FatalError(SImageError + Name);
  // converting image in the current color mode
  Result := SDL_DisplayFormat(Result);
  // setting image transparency
  SDL_SetColorKey(Result, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
    SDL_MapRGB(Result.Format, $FF, 0, $FF));
end;

procedure TGame.DrawBackground;
var
  mx, my: SmallInt;
begin
  for my := 0 to MapHeight - 1 do
    for mx := 0 to MapWidth + MapInfoWidth - 1 do
      with FMap[my, mx] do
      begin
        if LevelDone and (Item in CollectedItems) then
          Item := tEmpty;
        DrawTile(mx, my, False);
      end;
  DrawMap;
  DrawInfo;
end;

procedure TGame.DrawTile(mx, my: SmallInt; Update: Boolean);
var
  l: TMapLayer;
  sr, dr: TSDL_Rect;
  t: TTileType;
begin
  sr.y := 0;
  sr.w := MapCellSize;
  sr.h := MapCellSize;
  dr.x := mx * MapCellSize;
  dr.y := my * MapCellSize;

  with Map[mx, my] do
    // drawing from the "back" up to the "item" layer
    for l := mlBack to mlItem do
    begin
      // getting tile from the coresponding map table
      t := TTileType(ifop(mx < MapWidth, Ord(z[l]), Ord(FInfo[my, mx].z[l])));
      if l = mlItem then
        // hidden items are not shown while having some weight
        // or shown as a "diamond"
        if t in HiddenItems then
          t := ifop(Weight > 0, tEmpty, tDiamond)
        // chest items are shown as a "chest"
        // until catching key starts or level is done
        else if t in ChestItems then
          t := ifop((GameState in (PersistentStates - [gsCatchKey])) and
            not LevelDone, tChest, ifop(Hero.InBasket(t), tEmpty, t));

      sr.x := Ord(t) * MapCellSize;

      if t <> tEmpty then
        SDL_BlitSurface(TileImage, @sr, Background, @dr);
    end;

  if Update then
    SDL_UpperBlit(Background, nil, Screen, nil);
end;

procedure TGame.DrawInfo;
var
  i: Byte;
  mx, my: SmallInt;
  s: string;
begin
  // drawing Hero's basket items
  if Hero <> nil then
  begin
    for i := 0 to 2 do
      FInfo[5, MapWidth + 1 + i].Mid := Hero.Basket[i];
    FInfo[3, MapWidth + 1].Item := ifop(Hero.Bullets > 0, tBullet, tNothing);
  end;

  for my := 1 to 8 do
    for mx := MapWidth to MapWidth + 4 do
      DrawTile(mx, my, False);

  WriteText('STUFF', (MapWidth + 2) * MapCellSize + GridCellSize, 2 * MapCellSize + 9);
//  WriteText('HINT', (MapWidth + 1) * MapCellSize + 16, 6 * MapCellSize + 9);

  // writing Hero's bullets amount
  if Hero.Bullets > 0 then
  begin
    Str(Hero.Bullets, s);
    WriteText(s, (MapWidth + 1) * MapCellSize + GridCellSize, 4 * MapCellSize);
  end;

  if LevelTimer > 0 then
  begin
    WriteText('TIME', (MapWidth + 2) * MapCellSize + GridCellSize, 6 * MapCellSize);
    Str(LevelTimer, s);
    WriteText(s, (MapWidth + 2) * MapCellSize + GridCellSize, 7 * MapCellSize);
  end;

  DrawTile(0, 0, True);
end;

procedure TGame.DrawMap;
var
  x, y: SmallInt;
  dr: TSDL_Rect;
  l: Boolean;
begin
  dr.w := 8;
  dr.h := 8;
  for y := 0 to LevelMapHeight - 1 do
    for x := 0 to LevelMapWidth - 1 do
    begin
      dr.x := (MapWidth + 1) * MapCellSize + 3 + x * 9;
      dr.y := 10 * MapCellSize + 3 + 81 - y * 9;
      l := (x = Level.x) and (y = Level.y);
      // drawing all passed levels and the current level
      if FLevelDone[Level.z, y, x] or l then
        SDL_FillRectAdd(Background, @dr,
          SDL_MapRGB(Background.format, ifop(l, $ff, $7f), $ff, $7f));
  end;
end;

function TGame.GetMap(mx, my: SmallInt): TMapCell;
begin
  Result := FMap[my, mx];
end;

procedure TGame.SetMap(mx, my: SmallInt; Layer: TMapLayer; Value: TTileType);
begin
  FMap[my, mx].z[Layer] := Value;
  DrawTile(mx, my, True);
end;

function TGame.TileCount(Tiles: TTiles; CountWeight: Boolean = False): Word;
var
  mx, my: Byte;
  ml: TMapLayer;
begin
  Result := 0;
  for ml := mlBack to mlItem do
    for my := 0 to MapHeight - 1 do
      for mx := 0 to MapWidth - 1 do
        with Map[mx, my] do
          if Z[ml] in Tiles then
            Inc(Result, ifop(CountWeight, Weight, 1));
end;

procedure TGame.ReplaceTiles;
var
  mx, my: Byte;
  i: ShortInt;
begin
  i := Exclude;
  for my := 0 to MapHeight - 1 do
    for mx := 0 to MapWidth - 1 do
      with FMap[my, mx] do
        if z[Layer] = STile then
        begin
          if (Exclude = -1) or (i <> 0) then
          begin
            z[Layer] := DTile;
            DrawTile(mx, my, False);
          end;
          Dec(i);
        end;
  DrawTile(0, 0, True);
end;

function TGame.SpriteIn(gx1, gx2, gy1, gy2: SmallInt;
  Exclude: TActiveSprite = nil): TActiveSprite;
var
  i: Word;
begin
  for i := 0 to SpriteEngine.Sprites.Count - 1 do
  begin
    Result := TActiveSprite(SpriteEngine.Sprites[i]);
    { TODO : optimize intersection check }
    if ((Exclude = nil) or (Result <> Exclude) {and (Result <> Hero)}) and
      (Result.State <> ssRecovering) and not (Result.SpriteType in Ammunition) and
      RectsIntersect(
        PixelToGrid(Result.x), PixelToGrid(Result.x + GridCellSize),
        PixelToGrid(Result.y), PixelToGrid(Result.y + GridCellSize),
        gx1, gx2, gy1, gy2) then Exit;
  end;
  Result := nil;
end;

procedure TGame.CreateSprites;
var
  x, y, mx, my: SmallInt;
  dx, dy: TDelta;
  st: TSpriteType;
  h: Boolean;
begin
  for my := 0 to MapHeight - 1 do
    for mx := 0 to MapWidth - 1 do
      with Map[mx, my] do
        // sprite can stand on an empty item only
        if (Item in SpriteHolders) and
          (Sprite and $8 = 0) then
        begin
          // screen coordinates
          x := mx * MapCellSize;
          y := my * MapCellSize;
          // initial turn
          dx := ItoDX[Sprite and $3];
          dy := ItoDY[Sprite and $3];
          if Sprite and $F0 = $F0 then
            Hero.Reset(x, y, dx, dy)
          else
          begin
            // sprite type
            st := TSpriteType(Sprite shr 4);
            // flag whether sprite is a level hint
            h := Boolean(Sprite and $4);
            if  st = sBox then
              TBox.Create(sBox, $FF, x, y, 0, 0, h)
            else
            // enemy sprite can be created only if level is not done yet
            if not LevelDone then
              case st of
                sDragon:
                  TDragon.Create(sDragon, 0, x, y, 0, 0, h);
                sSkull:
                  TSkull.Create(sSkull, $FF, x, y, dx, dy, h);
                sRoller:
                  TRoller.Create(sRoller, 3, x, y, dx, dy, h);
                sFireBall:
                  TFireball.Create(sFireball, 0, x, y, dx, dy, h);
                sSleepy:
                  TSleepy.Create(x, y, dx, dy, h);
                sMedusa:
                  TMedusa.Create(sMedusa, $FF, x, y, 0, 0);
                sStony:
                  TStony.Create(sStony, 3, x, y, dx, dy, h);
                sCuckold:
                  TCuckold.Create(sCuckold, $FF, x, y, dx, dy, h);
              end
            else if h then
              case st of
                sSleepy:
                  if dx = 0 then
                    TSleepy.DoHint;
                sFireball:
                  TFireball.DoHint;
              end;
          end;
        end;
end;

procedure TGame.DeleteSprites;
var
  i: Word;
begin
  // delete all sprites BUT
  // - do not delete Hero
  // - delete boxes only if state is gsNextLevel or gsKillHero
  for i := SpriteEngine.Sprites.Count - 1 downto 0 do
    with SpriteEngine, TActiveSprite(Sprites[i]) do
      if (SpriteType <> sHero) and (All or (SpriteType <> sBox)) then
        RemoveSprite(Sprites[i]);
end;

procedure TGame.OpenChest;
begin
  GameState := gsCatchKey;
  DrawBackground;
end;

procedure TGame.OpenDoors(aState: TGameState);
begin
  ReplaceTiles(tDoorT, tDoorTO);
  ReplaceTiles(tDoorL, tDoorLO);
  ReplaceTiles(tDoorR, tDoorRO);
  ReplaceTiles(tDoorB, tDoorBO);
  ReplaceTiles(tDoorD, tDoorDO);
  ReplaceTiles(tDoorU, tDoorUO);
  // delete all enemy sprites in the level
  DeleteSprites(False);
  // mark level as done
  LevelDone := True;
  GameState := aState;
end;

procedure TGame.SetNextLevel(dx, dy, dz: TDelta);
begin
  // calculate the next level number
  Inc(Level.x, dx);
  Inc(Level.y, -dy);
  Inc(Level.z, dz);
end;

procedure TGame.NextLevel;
var
  i: Word;
  mx, my: SmallInt;
begin
  // delete all remaining sprites in the level
  GameState := gsNextLevel;
  DeleteSprites(True);
  // form the new level
  LoadMap(FMap, SizeOf(FMap));
  i := TileCount([tDoorD]);
  if i > 1 then
    ReplaceTiles(tDoorD, tEmpty, mlItem, Random(i));
  LevelTimer := TileCount([tMasterKey], True);
  GameState := gsStartLevel;
  DrawBackground;
  if TileCount(ChestItems) = 0 then
    LevelDone := True;
  if LevelDone then
    OpenDoors(gsStartLevel);
  // adjust the hero position
  with Hero do
  begin
    oX := x;
    oY := y;
    odX := dX;
    odY := dY;

    Reset(
      ifop(dX <> 0, (MapWidth - 1 - PixelToMap(x)) * MapCellSize, x),
      ifop(dY <> 0, (MapHeight - 1 - PixelToMap(y)) * MapCellSize, y), dX, dY);

    mx := PixelToMap(x);
    my := PixelToMap(y);
    with Map[mx, my] do
    begin
      // if Hero appears on the ground
      // - drawing an open door
      if Back in [tNothing, tEmpty] then
      begin
        SetMap(mx, my, mlItem, TTileType(Ord(tDoorTO) + DeltaToIndex(-dX, -dY)));
        // make one more grid cell step
        Inc(x, GridCellSize * dX);
        Inc(y, GridCellSize * dY);
      end
      // if Hero appears on the water
      // - recreating a swimming raft for Hero
      else if Back = tWater then
      begin
        if Item <> tEmpty then
        begin
          // make one more map cell step
          Inc(x, MapCellSize * dX);
          Inc(y, MapCellSize * dY);
        end;
        Sail := TShip.Create(Hero, x, y);
      end;
    end;
  end;
  CreateSprites;
end;

procedure TGame.KillHero;
begin
  PlaySound(sndKill);
  Hero.Reset(ox, oy, odx, ody);
  SetNextLevel(0, 0, 0);
  NextLevel;
end;

procedure TGame.PlaySound;
begin
  if FSounds[Sound] <> nil then
    Mix_PlayChannel(-1, FSounds[Sound], 0);
end;

procedure TGame.CheckHidden;
var
  mx, my: Byte;
begin
  for my := 0 to MapHeight - 1 do
    for mx := 0 to MapWidth - 1 do
      with FMap[my, mx] do
        if Item in HiddenItems then
        begin
          if Weight > 0 then
            Dec(Weight, ifop(Unhide, Weight, 1));
          if Weight = 0 then
          begin
            PlaySound(sndUnhide);
            DrawTile(mx, my, True);
          end;
        end;
end;

procedure TGame.WriteText(const Text: string; aX, aY: SmallInt);
var
  i: Word;
  srcRect, destRect: TSDL_Rect;
begin
  srcRect.y := 0;
  srcRect.w := 14;
  srcRect.h := 16;
  destRect.y := aY;
  for i := 1 to Length(Text) do
  begin
    srcRect.x := (Ord(Text[i]) - Ord(' ')) * 16 + 1;
    destRect.x := aX - Length(Text) * 14 div 2 + (i - 1) * 14;
    SDL_BlitSurface(FontImage, @srcRect, Background, @destRect);
  end;
end;

procedure TGame.LoadMap(var Map; Length: Word);
var
  f: File;

function GetOffset: Longint;
var
  s: string;
  c: Longint;
begin
  Result := FileSize(f);
  Str(Level.x * 100 + Level.y + 10101, s);
  s[1] := Chr(Ord(FStartLevel[1]) + Level.z);
  s := FIniFile.ReadString(SGenSection, s, '');
  if s = '' then Exit;

  Val(s, Result, c);
  if c = 0 then Exit;
  if ParseLevel(s) then
    Result := GetOffset;
end;

begin
  AssignFile(f, SMapDir + SMapFile);
  try
    Reset(f, 1);
  except
    FatalError(SMapOpenError);
  end;
  try
    try
      if @Map <> @FInfo then
        Seek(f, SizeOf(FInfo) + GetOffset);
      BlockRead(f, Map, Length);
    except
      FatalError(SMapReadError);
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TGame.LoadGame;
var
  f: File of TGameRecord;
  r: TGameRecord;
begin
  AssignFile(f, SGameStateFile);
  Reset(f);
  try
    Read(f, r);
    FLevelDone := r.LevelDone;
    Level := r.Level;
    ox := r.x;
    oy := r.y;
    odx := r.dx;
    ody := r.dy;
    GameState := gsKillHero;
  finally
    CloseFile(f);
  end;
end;

procedure TGame.SaveGame;
var
  f: File of TGameRecord;
  r: TGameRecord;
begin
  AssignFile(f, SGameStateFile);
  Rewrite(f);
  try
    r.LevelDone := FLevelDone;
    r.Level := Level;
    r.x := ox;
    r.y := oy;
    r.dx := odx;
    r.dy := ody;
    Write(f, r);
  finally
    CloseFile(f);
  end;
end;

function TGame.GetLevelDone: Boolean;
begin
  Result := FLevelDone[Level.z, Level.y, Level.x];
end;

procedure TGame.SetLevelDone(Value: Boolean);
begin
  FLevelDone[Level.z, Level.y, Level.x] := Value;
end;

function TGame.ParseLevel(const S: string): Boolean;
var
  c: Longint;
begin
  Result := False;
  Val(s[2] + s[3], Level.x, c);
  if c <> 0 then Exit;
  Val(s[4] + S[5], Level.y, c);
  if c <> 0 then Exit;
  Dec(Level.x);
  Dec(Level.y);
  Level.z := Ord(s[1]) - Ord(FStartLevel[1]);
  Result := True;
end;

procedure TGame.SetLevelTimer(Value: Smallint);
begin
  FLevelTimer := Value;
  DrawInfo;
end;

end.

