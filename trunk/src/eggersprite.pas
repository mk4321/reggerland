{ Unit EggerSprite implements all sprite classes

  @Author  Alexey Yarochkin
  @Version 07.12.2010 v0.2
  @History 27.01.2005 v0.1
}

unit EggerSprite;

interface

uses
  SDL, SDLSprites, EggerData;

type
  TActiveSprite = class;

  /// Map cell record
  TCellInfo = record
    /// Sprites in front
    /// [0..1] - sprites on each grid cell of the map cell
    /// [2] - sprite on both grid cells (if [0] = [1])
    Sprite: array [0..2] of TActiveSprite;
    /// map cell data on each grid cell
    Cell: array [0..1] of TMapCell;
  end;

  /// Abstract ancestor for all sprite classes
  TActiveSprite = class(TSprite)
  private
    /// Actual sprite state
    /// Note: some values may have different meaning for different sprite types
    FState: TSpriteState;
    ///
    /// True if the sprite should behave uncommonly
    FLevelHint: Boolean;
    LoopMask: Byte;
    ///
    /// True if the sprite has released a bullet, which is moving now
    isShooting: Boolean;
    CellInfo: TCellInfo;
    /// Original sprite position
    /// Used to reposition it when recovering from the death
    Xorig, Yorig: TPixCoord;
    /// Original sprite direction
    /// Used to redirect it when recovering from the death or pause
    dXorig, dYorig: TDelta;
    /// Collect all data of the map cell in front
    procedure GetItemsInFront(var Info: TCellInfo); virtual;
    /// Check if there is a hurdle in front
    function HurdleInFront: Boolean; virtual;
    /// Check if the sprite can step on the given map cell
    function CanStepOn(const Info: TCellInfo): Boolean; virtual;
    /// Calculate next animation phase
    function GetPhase(Next: Boolean): Word; virtual;
    /// Define the current sprite speed
    function GetSpeed: TSpeed; virtual;
    /// Start the attack (shooting)
    procedure Attack(adx, ady: TDelta); virtual;
    /// Make sprite hurt
    procedure Hurt(adx, ady: TDelta); virtual;
    /// Direct the sprite
    procedure DirectTo(adx, ady: TDelta);
    /// Check if the sprite is moving now
    function Moving: Boolean;
    /// Check if the sprite is staying entirely on one grid cell
    function InGCell: Boolean;
    /// Check if the sprite is staying entirely on one map cell
    function InMCell: Boolean;
    procedure SetLevelHint(const Value: Boolean);
    procedure SetState(const Value: TSpriteState); virtual;
    /// Start the process of recovery
    procedure Recover;
  public
    /// Actual sprite type
    SpriteType: TSpriteType;
    /// Current velocity
    vX, vY: TSpeed;
    /// Current direction
    dX, dY: TDelta;
    /// Timer ticking when sprite is paused (e.g. PauseTime)
    PauseTime: Word;
    /// Timer ticking when sprite is recovering (after death)
    RecoveryTime: Word;
    /// Sprite used by this one as a sailing vehicle
    Sail: TActiveSprite;
    /// Sprite which uses this one as a sailing vehicle
    Sailor: TActiveSprite;

    /// Create, position and direct a sprite object
    constructor Create(aType: TSpriteType; aMask: Byte; aX, aY: TPixCoord;
      adX: TDelta = 0; adY: TDelta = 0; aHint: Boolean = False);
    /// Destroy and free the object
    procedure Free; override;
    /// Dispatch the sprite moves
    procedure Move; override;
    procedure Live; virtual; abstract;
    /// Try to push sprite in the given direction
    function Pushed(adx, ady: TDelta): Boolean;
    /// Draw sprite
    procedure Draw; override;

    property LevelHint: Boolean read FLevelHint write SetLevelHint;
    property State: TSpriteState read FState write SetState;
  end;

  /// Hero sprite
  THero = class(TActiveSprite)
  private
    FBullets: ShortInt;
    FGhostly: Boolean;
    function HurdleInFront: Boolean; override;
    function CanStepOn(const Info: TCellInfo): Boolean; override;
    procedure SetBullets(const Value: ShortInt);
    procedure SetGhostly(const Value: Boolean);
    function GetSpeed: TSpeed; override;
  public
    Basket: array [0..2] of TTileType;
    procedure Live; override;
    procedure Draw; override;
    procedure Attack(adx, ady: TDelta); override;
    procedure Reset(aX, aY: TPixCoord; adX, adY: TDelta);
    function InBasket(Tile: TTileType; Take: Boolean = False): Boolean;
    procedure PutInBasket(Tile: TTileType);
    property Bullets: ShortInt read FBullets write SetBullets;
    property Ghostly: Boolean read FGhostly write SetGhostly;
  end;

  /// Staying dragon sprite
  /// The dragon watches the Hero
  TDragon = class(TActiveSprite)
  private
    function GetPhase(Next: Boolean): Word; override;
  public
    procedure Live; override;
  end;

  /// Staying fireball sprite
  /// The fireball ...
  TFireball = class(TActiveSprite)
  private
    function GetPhase(Next: Boolean): Word; override;
    procedure SetState(const Value: TSpriteState); override;
  public
    procedure Live; override;
    class procedure DoHint;
  end;

  TMedusa = class(TActiveSprite)
  private
    function GetPhase(Next: Boolean): Word; override;
  public
    procedure Live; override;
  end;

  TBullet = class(TActiveSprite)
  private
    Shooter: TActiveSprite;
    function CanHitTarget: Boolean;
    function CanStepOn(const Info: TCellInfo): Boolean; override;
    function GetSpeed: TSpeed; override;
  public
    constructor Create(sType: TSpriteType; aShooter: TActiveSprite;
      adx, ady: TDelta; CheckPath: Boolean); reintroduce;
    procedure Live; override;
  end;

  TBox = class(TActiveSprite)
  private
    function CanStepOn(const Info: TCellInfo): Boolean; override;
  public
    procedure Live; override;
  end;

  TShip = class(TActiveSprite)
  private
    function GetPhase(Next: Boolean): Word; override;
  public
    constructor Create(aSailor: TActiveSprite; aX, aY: TPixCoord); reintroduce;
    procedure Live; override;
  end;

  TSwingingSprite = class(TActiveSprite)
  public
    procedure Live; override;
  end;

  TCuckold = class(TSwingingSprite)
  private
    function GetPhase(Next: Boolean): Word; override;
  public
    procedure Live; override;
  end;

  TRoamingSprite = class(TActiveSprite)
  private
    procedure GetItemsInFront(var Info: TCellInfo); override;
  public
    procedure Live; override;
    function HeroTouched(adX, adY: TDistance): Boolean; virtual;
  end;

  TSkull = class(TRoamingSprite)
  private
    function GetPhase(Next: Boolean): Word; override;
  public
    procedure Live; override;
    function HeroTouched(adX, adY: TDistance): Boolean; override;
  end;

  TRoller = class(TRoamingSprite)
  private
    function GetPhase(Next: Boolean): Word; override;
  public
    procedure Live; override;
  end;

  TSleepy = class(TRoamingSprite)
  private
    Touched: Byte;
    function GetPhase(Next: Boolean): Word; override;
    function GetSpeed: TSpeed; override;
    procedure Hurt(adx, ady: TDelta); override;
    procedure SetState(const Value: TSpriteState); override;
  public
    constructor Create(aX, aY: TPixCoord; adX: TDelta = 0; adY: TDelta = 0;
      aLevelHint: Boolean = False); reintroduce;
    procedure Live; override;
    function HeroTouched(adX, adY: TDistance): Boolean; override;
    procedure TouchMe;
    class procedure DoHint;
  end;

  TStony = class(TRoamingSprite)
  private
    function HurdleInFront: Boolean; override;
    function GetSpeed: TSpeed; override;
  public
    function HeroTouched(adX, adY: TDistance): Boolean; override;
  end;

implementation

uses
  EggerGame;

{ TActiveSprite }

{ @ Create, position and direct a new sprite object

  @param aType  actual sprite type
  @param aMask  binary mask of animation phases
  @param ax     initial x-coordinate (in pixels)
  @param ay     initial y-coordinate (in pixels)
  @param adx    initial x-direction
                [ 0] not directed horizontally
                [ 1] directed to the right
                [-1] directed to the left
  @param ady    initial y-direction
                [ 0] not directed vertically
                [ 1] directed to the
                [-1] directed to the
  @param aHint  indicates if the sprite should behave uncommonly
                if True the sprite is used as a hint in the current level
}

constructor TActiveSprite.Create(aType: TSpriteType; aMask: Byte; aX, aY: TPixCoord;
  adX: TDelta = 0; adY: TDelta = 0; aHint: Boolean = False);
begin
  inherited Create('', MapCellSize, MapCellSize);

  SpriteType := aType;
  Game.SpriteEngine.AddSprite(Self);
  // original coordinates are needed for the recovery
  Xorig := aX;
  Yorig := aY;
  dXorig := adX;
  dYorig := adY;
  // position the sprite to the original coordinates
  Recover;
  State := ssNormal;
  /// Sprites are arranged in 3 z-layers:
  /// * 3 (highest): Hero
  /// * 0 (lowest): ammunition
  /// * 1 (middle): all others
  Z := ifop(aType = sHero, 3, ifop(aType in Ammunition, 0, 1));
  LoopMask := aMask;
  AnimPhase := GetPhase(False);
  LevelHint := aHint;
end; { Create }

{ @ Destroy and free the object
}

procedure TActiveSprite.Free;
begin
  // sprite image must not be released automatically
  Image := nil;
  inherited;
end; { Free }

{ @ Dispatch the sprite moves
}

procedure TActiveSprite.Move;
var
  b1, b2: TTileType;
  sp: TSpeed;
  d: SmallInt;
  AdjustSailor: Boolean;

  { Counts down pause time
    If the time is out:
     - the sprite returns to the normal state
     - if the sprite was swimming it sinks and recovers
     - if the sprite sinks its sailor dies
  }
  procedure CheckPaused;
  begin
    Dec(PauseTime);
    if PauseTime = 0 then
      if State = ssSwimming then
      // if sprite was swimming
      begin
        // start recovery
        Recover;
        // kill sailing Hero
        if Sailor = Game.Hero then
          Game.GameState := gsKillHero;
      end
      else
      // if sprite was not swimming
      begin
        // restore original direction (actual for fireballs)
        DirectTo(dXorig, dYorig);
        // return to the normal state
        State := ssNormal;
      end;
  end; { CheckPaused }

  { Counts down recovery time
    If the sprite position is occupied by someone (not Hero):
     - tries to reposition the recovering sprite
       (searches for a free placeholder of a sprite of the same type)
     - if all places are occupied, kills the sprite (so it does not appear)
    If the time is out, the sprite returns to the normal state
  }
  procedure CheckRecovery;
  var
    MapX, MapY: TMapCoord;
    GridX, GridY: TGridCoord;
    Sprite: TActiveSprite;
  begin
    GridX := PixToGrid(X);
    GridY := PixToGrid(Y);
    // look for a sprite in the same place excluding itself
    Sprite := Game.SpriteIn(GridX, GridX + 1, GridY, GridY + 1, Self);
    // if no sprites occupy this cell (Hero is not counted as an occupant)
    if (Sprite = nil) or (Sprite.SpriteType = sHero) then
    begin
      Dec(RecoveryTime);
      if RecoveryTime = 0 then
        State := ssNormal;
    end
    else
    // if the current placeholder is occupied
    begin
      for MapY := 0 to MapHeight - 1 do
        for MapX := 0 to MapWidth - 1 do
          with Game.Map[MapX, MapY] do
          begin
            GridX := MapToGrid(MapX);
            GridY := MapToGrid(MapY);
            // if the cell is for Sprite of the same type and it's free
            if (Item = tEmpty) and (TSpriteType(Sprite shr 4) = SpriteType) and
              (Game.SpriteIn(GridX, GridX + 1, GridY, GridY + 1) = nil) then
              begin
                // reposition the Sprite and start recovery again
                Xorig := MapToPix(MapX);
                Yorig := MapToPix(MapY);
                Recover;
                Exit;
              end;
          end;
      // no free placeholders found - removing the Sprite
      Kill;
    end;
  end; { CheckRecovery }

begin { Move }
  // pause time count down
  if PauseTime > 0 then
    CheckPaused;
  // recovery time count down (Hero cannot recover)
  if (RecoveryTime > 0) and (SpriteType <> sHero) then
    CheckRecovery;

  // set next animation picture
  AnimPhase := GetPhase(True);
  // if some time has passed since sprite is swimming it may start drifting
  if (State = ssSwimming) and not Moving and (PauseTime < PauseTimeMax - 28) then
  begin
    // check if water in the current cell has a flow (d is its direction)
    d := Game.Map[PixToMap(x), PixToMap(y)].Weight - 1;
    // trying to push sprite in that direction
    if (d >= 0) and Pushed(ItoDX[d], ItoDY[d]) then
    begin
      // reset swimming state (mainly the PauseTime time)
      State := ssSwimming;
      AnimPhase := GetPhase(False);
    end;
  end;

  // let the sprite perform its normal living functions:
  // - checking the map
  // - redirecting itself
  if State in [ssNormal, ssSleeping] then
    Live;

  sp := GetSpeed;
  // if the sprite is transporting another one (as a sail)
  // the sailor coordinates must be adjusted (if not hibernated by enemy)
  AdjustSailor := (Sailor <> nil) and (Sailor.State <> ssSleeping);

  // adjust self coordintates according to the velocity
  if vX <> 0 then
  begin
    Inc(x, sp * dX);
    Dec(vX, sp * dX);
    // if the sprite is moving horizontally and its wakeful sailor stands on
    if AdjustSailor and (Sailor.y = y) then
      Sailor.x := x;
  end
  else if vY <> 0 then
  begin
    Inc(y, sp * dY);
    Dec(vY, sp * dY);
    // if the sprite is moving vertically and its wakeful sailor stands on
    if AdjustSailor and (Sailor.x = x) then
      Sailor.y := y;
  end;

  if not Moving then
  begin
    b1 := Game.Map[PixToMap(x), PixToMap(y)].Back;
    b2 := Game.Map[PixToMap(x + GridCellSize), PixToMap(y + GridCellSize)].Back;
    // if the sprite stops on a liquid surface it starts swimming
    // - Hero cannot swim - he can only sail
    // - ammunition does not swim - it flies above
    if (State <> ssSwimming) and not (SpriteType in [sHero] + Ammunition) and
      InGCell and ([b1, b2] <= LiquidItems) then
        State := ssSwimming;
    // if Hero stops on ice he continues to slide further
    if (SpriteType = sHero) and (tIce in [b1, b2]) then
      Pushed(dX, dY);
  end;
end; { Move }

{ @ Try to push sprite in the given direction

  @param adx  x-direction of the push
  @param ady  y-direction of the push

  @return     True if pushed successfully
}

function TActiveSprite.Pushed(adx, ady: TDelta): Boolean;
var
  v: Byte;
  j: Boolean;
begin
  Result := False;
  // if still moving => no push allowed
  if Moving or not InGCell then Exit;

  DirectTo(adx, ady);
  // check if destination cell is empty (no hurdle in front)
  j := HurdleInFront;
  // prospected velocity:
  // - moving across grid cells
  // - swimminig across map cells (if adjusted to the map cell)
  v := ifop((State = ssSwimming) and InMCell, MapCellSize, GridCellSize);
  with CellInfo do
    // if the destination is not passable
    if j then
      // there is an exception:
      // if egg is getting into a liquid surface it must pass the whole map cell
      if ([Cell[0].Back, Cell[1].Back] <= LiquidItems) and
        (State in EggStates) then
          v := MapCellSize
      // no other reason to allow the push
      else Exit
    // if there is a sprite in front, which is swimming:
    // - jump on it across the whole map call
    // - register as a sailor and start sailing
    else if (Sprite[2] <> nil) and (Sprite[2].State = ssSwimming) then
    begin
      Sail := Sprite[2];
      Sail.Sailor := Self;
      // set to skip the next check (jumping off the sail)
      j := True;
      v := MapCellSize;
    end;

  // if pushed off while sailing
  // - unregister sailor
  // - jump off across the whole map cell
  if not j and (Sail <> nil) then
    with Game, Sail do
    begin
      Sailor := nil;
      // remove sail if it was a raft
      if SpriteType = sRaft then
      begin
        Kill;
        // show the original raft item back on the map
        if LevelStatus = lsDone then
          GameState := gsCatchKey;
        DrawBackground;
      end;
      Self.Sail := nil;
      v := MapCellSize;
    end;

  // set velocity for each x/y component
  vX := dX * v;
  vY := dY * v;
  Result := True;
end; { Pushed }

{ @ Check if there is a hurdle in front
    A hurdle may be a blocking tile or a sprite

  @return  True if the way is jammed
}

function TActiveSprite.HurdleInFront: Boolean;

function None(sp: TActiveSprite): Boolean;
begin
  Result := (sp = nil) or (sp = Self);
end; { None }

begin { HurdleInFront }
  Result := False;
  if not InGCell then Exit;

  GetItemsInFront(CellInfo);
  with CellInfo do
    Result := not (CanStepOn(CellInfo) and None(Sprite[0]) and None(Sprite[1]));
end; { HurdleInFront }

{ @ Collect all data of the map cell in front

  @param Info  map cell record to fill in

  @return      Info
}

procedure TActiveSprite.GetItemsInFront(var Info: TCellInfo);
var
  GridX1, GridX2, GridY1, GridY2: TGridCoord;
begin
  GridX1 := PixToGrid(x) + ifop(dX = 0, 0, ifop(dX > 0, GridsInMap, -1));
  GridX2 := GridX1 + ifop(dX = 0, GridsInMap - 1, 0);
  GridY1 := PixToGrid(y) + ifop(dY = 0, 0, ifop(dY > 0, GridsInMap, -1));
  GridY2 := GridY1 + ifop(dY = 0, GridsInMap - 1, 0);

  Info.Sprite[0] := Game.SpriteIn(GridX1, GridX1, GridY1, GridY1, Game.Hero.Sail);
  Info.Sprite[1] := Game.SpriteIn(GridX2, GridX2, GridY2, GridY2, Game.Hero.Sail);
  if Info.Sprite[0] = Self then
    Info.Sprite[0] := nil;
  if Info.Sprite[1] = Self then
    Info.Sprite[1] := nil;
  if Info.Sprite[0] = Info.Sprite[1] then
    Info.Sprite[2] := Info.Sprite[0]
  else
    Info.Sprite[2] := nil;
  Info.Cell[0] := Game.Map[GridToMap(GridX1), GridToMap(GridY1)];
  Info.Cell[1] := Game.Map[GridToMap(GridX2), GridToMap(GridY2)];
end; { GetItemsInFront }

{ @ Check if the sprite can step on the given map cell
    This function checks if there is no blocking tiles in front of the sprite

  @param Info  map cell record

  @return      True if cell is free for stepping on
}

function TActiveSprite.CanStepOn(const Info: TCellInfo): Boolean;
var
  Backs, Items: TTiles;
begin
  if State = ssSwimming then
  begin
    Backs := LiquidItems;
    Items := HeroPathItems;
  end
  else
  begin
    Backs := AnyPathBacks;
    Items := AnyPathItems;
    if State = ssPaused then
      Include(Backs, tRoad);
  end;
  with Info do
    Result := ([Cell[0].Back, Cell[1].Back] <= Backs) and
      ([Cell[0].Item, Cell[1].Item] <= Items);
end; { CanStepOn }

{ @ Draw sprite
}

procedure TActiveSprite.Draw;
begin
  if (State in EggStates) and (SpriteType <> sRaft) then
    Image := Game.SpriteImage[sEgg]
  else
    Image := Game.SpriteImage[SpriteType];

  SDL_SetAlpha(Image, SDL_SRCALPHA,
    255 - ifop(RecoveryTime <= $C0, (RecoveryTime + $3F) and $C0, 255));

  inherited;
end; { Draw }

{ @ Make sprite hurt
    The sprite recovers if it is already PauseTime
}

procedure TActiveSprite.Hurt;
begin
  if State = ssPaused then
    Recover
  else
    State := ssPaused;
end; { Hurt }

{ @ Calculate next animation phase

  @param Next  identifies whether to calculate the next (or initial) phase

  @return      index of the animation phase
}

function TActiveSprite.GetPhase(Next: Boolean): Word;
begin
  Next := Next and Moving;
//  Result :=
//    ifop(State = ssPaused, 3 - PauseTime shr 5,
//    ifop(State = ssSwimming, 7 - PauseTime shr 5,
//    ifop(LoopMask = $FF, 0, DeltaToIndex(dX, dY) * (LoopMask + 1) +
//      ((Game.MovePhase * Ord(Next)) and LoopMask))));
  if State = ssPaused then
    Result := 3 - PauseTime shr 5
  else if State = ssSwimming then
    Result := 7 - PauseTime shr 5
  else if LoopMask = $FF then
    Result := 0
  else
    Result := DeltaToIndex(dX, dY) * (LoopMask + 1) +
      (Byte(Game.MovePhase * Ord(Next)) and LoopMask);
end; { GetPhase }

{ @ Start the attack (shooting)
}

procedure TActiveSprite.Attack;
var
  st: TSpriteType;
begin
  if isShooting or not InGCell then Exit;
  case SpriteType of
    sHero: st := sBullet;
    sFireball: st := sFire;
    sMedusa: st := sFlash;
    sCuckold: st := sKnife;
  else
    st := sEgg;
  end;
  TBullet.Create(st, Self, adx, ady, not (SpriteType in [sHero, sFireball]));
end; { Attack }

{ @ Check if the sprite is moving now

  @return  True if the sprite is moving now
}

function TActiveSprite.Moving: Boolean;
begin
  Result := (vX <> 0) or (vY <> 0);
end; { Moving }

{ @ Check if the sprite is staying entirely on one grid cell

  @return  True if sprite is on a grid cell
}

function TActiveSprite.InGCell: Boolean;
begin
  Result := (x mod GridCellSize = 0) and (y mod GridCellSize = 0);
end; { InGCell }

{ @ Check if the sprite is staying entirely on one map cell

  @return  True if sprite is on a map cell
}

function TActiveSprite.InMCell: Boolean;
begin
  Result := (x mod MapCellSize = 0) and (y mod MapCellSize = 0);
end; { InMCell }

{ @ Start the process of recovery
}

procedure TActiveSprite.Recover;
begin
  x := Xorig;
  y := Yorig;
  DirectTo(dXorig, dYorig);
  State := ssRecovering;
end; { Recover }

{ @ Define the current sprite speed

  @return  current speed factor
}

function TActiveSprite.GetSpeed: TSpeed;
begin
  Result :=
    ifop(State = ssRecovering, 0,
    ifop(State = ssSwimming, 1, 4));
end; { GetSpeed }

{ @ Direct the sprite

  @param adx  x-direction
  @param ady  y-direction
}

procedure TActiveSprite.DirectTo(adx, ady: TDelta);
begin
  dX := adx;
  dY := ady;
end; { DirectTo }

procedure TActiveSprite.SetState(const Value: TSpriteState);
begin
  FState := Value;
  PauseTime := ifop((SpriteType <> sHero) and (Value in EggStates),
    PauseTimeMax, 0);
  RecoveryTime := ifop(Value = ssRecovering, RecoveryTimeMax, 0);
//  SendDebug(GetEnumName(TypeInfo(TSpriteType), Ord(SpriteType)) + ': ' +
//    GetEnumName(TypeInfo(TSpriteState), Ord(Value)));
end; { SetState }

procedure TActiveSprite.SetLevelHint(const Value: Boolean);
begin
  FLevelHint := Value;
end; { SetLevelHint }

{ THero }

procedure THero.Live;
var
  MapX, MapY: TMapCoord;
begin
  if (State = ssSleeping) or Ghostly or not InMCell then Exit;

  MapX := PixToMap(X);
  MapY := PixToMap(Y);
  with Game, Map[MapX, MapY] do
  begin
    case Item of

      tHeart, tHeartInf:
        begin
          CheckHidden;

          Bullets := ifop(Item = tHeart, Bullets + Weight, -1);
          if Weight > 0 then
            PlaySound(sndRecharge)
          else
            PlaySound(sndGold);

          SetMap(MapX, MapY, mlItem, tEmpty);
          if TileCount([tHeart]) = 0 then
            GameState := gsOpenChest;
        end;

      tKey, tMasterkey, tRaft:
        if GameState = gsCatchKey then
        begin
          CheckHidden;
          PlaySound(sndKey);
          GameState := gsOpenDoors;
          LevelTimer := 0;
          if Item = tRaft then
          begin
            Hero.PutInBasket(Item);
            Game.DrawTile(MapX, MapY, True);
          end
          else
            SetMap(MapX, MapY, mlItem, tEmpty);
        end
        else if (Weight > 0) and (TileCount([tHeart]) = Weight) then
          CheckHidden(True);

      tDoorOpenInside..tDoorOpenUp:
        begin
          vX := 0;
          vY := 0;
          if Sail <> nil then
            DirectTo(Sail.dX, Sail.dY);
          if Back <> tEmpty then
            if Item in [tDoorOpenDown, tDoorOpenUp] then
              SetNextLevel(0, 0, ifop(Item = tDoorOpenDown, -1, 1))
            else
              SetNextLevel(dX, dY, 0);
          GameState := gsNextLevel;
          SetMap(MapX, MapY, mlItem, tEmpty);
        end;

      tBridgeVertical, tSpanner..tHammer, tBoxer, tEraser:
        if Weight = 0 then
        begin
          PlaySound(sndGold);
          Hero.PutInBasket(Item);
          SetMap(MapX, MapY, mlItem, tEmpty);
        end;
    end;
  end;
end; { Live }

function THero.HurdleInFront: Boolean;
var
  c: Boolean;
begin
  with CellInfo do
  begin
    Result := inherited HurdleInFront;
    c := CanStepOn(CellInfo);

    if Ghostly then
      Result := not c
    else if Sprite[2] <> nil then
      with Sprite[2] do
        case State of
          ssPaused:
            Result := not c or not (Moving or Pushed(Self.dX, Self.dY));
          ssSwimming:
            Result := False;
        else
          case SpriteType of
            sBox:
              Result := not (c and Pushed(Self.dX, Self.dY));
            sSleepy:
              TSleepy(Sprite[2]).TouchMe;
          end;
        end;
  end;
end; { HurdleInFront }

function THero.CanStepOn(const Info: TCellInfo): Boolean;
var
  Backs, Items: TTiles;
begin
  if Ghostly then
  begin
    Backs := GhostPathBacks;
    Items := GhostPathItems;
  end
  else
  begin
    Backs := HeroPathBacks;
    Items := HeroPathItems;
    if (dX <> 0) and (x mod MapCellSize = 0) or (dY <> 0) and (y mod MapCellSize = 0) then
      Exclude(Items, TTileType(Ord(tArrowDown) - DeltaToIndex(dX, dY)));
    if (dY > 0) then
      Exclude(Items, tDoorOpenUp);
  end;

  with Info do
    Result := ([Cell[0].Back, Cell[1].Back] <= Backs) and
      ([Cell[0].Item, Cell[1].Item] <= Items);
end; { CanStepOn }

procedure THero.Attack;
const
  CNextArrow: array [tArrowUp..tArrowDown] of TTileType =
    (tArrowRight, tArrowUp, tArrowDown, tArrowLeft);
var
  MapX, MapY: TMapCoord;
  Box: TActiveSprite;

  procedure Cast(Tile: TTileType);
  var
    i: Word;
  begin
    with Game.SpriteEngine do
      for i := 0 to Sprites.Count - 1 do
        with TActiveSprite(Sprites[i]) do
          case Tile of
            tHourglass:
              if not (SpriteType in [sHero, sBox]) then
              begin
                State := ssRecovering;
                RecoveryTime := RecoveryTime shr 2;
              end;
            tHypnotic:
              if SpriteType in [sMedusa, sCuckold] then
                State := ssSleeping;
          end;

  end; { Cast }

  function BoxInDirection: Boolean;
  var
    i: Word;
  begin
    Result := True;
    with Game.SpriteEngine do
      for i := 0 to Sprites.Count - 1 do
      begin
        Box := TActiveSprite(Sprites[i]);
        if (Box.SpriteType = sBox) and
          (sgn(Box.x - x) = dX) and (sgn(Box.y - y) = dY) then Exit;
      end;
    Box := nil;
    Result := False;
  end; { BoxInDirection }

begin { Attack }
  MapX := PixToMap(x) + dX;
  MapY := PixToMap(y) + dY;
  with Game, Map[MapX, MapY] do
    if Ghostly then
      Ghostly := False

    else if InMCell and (Item in FragileItems) and
      (InBasket(tHammer, True) or InBasket(tEraser, True)) then
        SetMap(MapX, MapY, mlItem, tEmpty)

    else if InGCell and InBasket(tBoxer, True) then
    begin
      with TBox.Create(sBox, $FF, x + GridToPix(dX), y + GridToPix(dY), dX, dY) do
        if not Pushed(dX, dY) then
          Kill;
    end

    else if InMCell and (Back in LiquidItems) and InBasket(tBridgeVertical, True) then
    begin
      SetMap(MapX, MapY, mlBack, tGrass);
      SetMap(MapX, MapY, mlMid, ifop(dX <> 0, tBridgeHorizontal, tBridgeVertical));
      if Sail <> nil then
        Pushed(dX, dY);
    end

    else if InMCell and (Item in ArrowItems) and InBasket(tSpanner, True) then
      SetMap(MapX, MapY, mlItem, CNextArrow[Item])

    else if InGCell and (Back = tWater) and InBasket(tRaft, True) then
    begin
      TShip.Create(nil, x + MapToPix(dX), y + MapToPix(dY));
      Pushed(dX, dY);
    end

    else if InBasket(tHourglass, True) then
      Cast(tHourglass)

    else if InBasket(tHypnotic, True) then
      Cast(tHypnotic)

    else if BoxInDirection and InBasket(tMagnet, True) then
    begin
      Box.DirectTo(-dX, -dY);
      Box.State := ssSleeping;
    end

    else if not isShooting and (Bullets <> 0) then
    begin
      inherited Attack(adx, ady);
      if isShooting then
        Bullets := Bullets - 1;
    end
    else Exit;

  Game.PlaySound(sndShoot);
end; { Attack }

procedure THero.Reset(aX, aY: TPixCoord; adX, adY: TDelta);
var
  i: Byte;
begin
  X := aX;
  Y := aY;
  DirectTo(adX, adY);
  vX := 0;
  vY := 0;
  AnimPhase := 0;
  isShooting := False;
  State := ssNormal;
  Sail := nil;
  Ghostly := False;
  Bullets := 0;
  for i := low(Basket) to high(Basket) do
    InBasket(Basket[i], True);
  AnimPhase := GetPhase(False);
end; { Reset }

function THero.InBasket(Tile: TTileType; Take: Boolean = False): Boolean;
var
  i: Byte;
begin
  Result := True;
  for i := 0 to 2 do
    if Basket[i] = Tile then
    begin
      if Take then
      begin
        Basket[i] := tNothing;
        Game.DrawInfo;
      end;
      Exit;
    end;
  Result := False;
end; { InBasket }

procedure THero.PutInBasket(Tile: TTileType);
var
  i: Byte;
begin
  for i := 0 to 2 do
    if Basket[i] = tNothing then
    begin
      Basket[i] := Tile;
      Game.DrawInfo;
      Exit;
    end;
end; { PutInBasket }

procedure THero.SetBullets(const Value: ShortInt);
begin
  FBullets := Value;
  Game.DrawInfo;
end; { SetBullets }

procedure THero.SetGhostly(const Value: Boolean);
var
  i: Byte;
begin
  if FGhostly = Value then Exit;
  FGhostly := Value;
  RecoveryTime := ifop(Value, 127, 0);
  if Value then
  begin
    if Sail <> nil then
      Sail.Sailor := nil;
    Sail := nil;
    Game.PlaySound(sndTrick);
  end
  else
  begin
    Dec(x, MapToPix(dX));
    Dec(y, MapToPix(dY));
    for i := 1 to GridsInMap do
    begin
      if HurdleInFront then
        Game.GameState := gsKillHero;
      Inc(x, GridToPix(dX));
      Inc(y, GridToPix(dY));
    end;
  end;
end; { SetGhostly }

function THero.GetSpeed: TSpeed;
begin
  Result := ifop(State = ssSleeping, 0, inherited GetSpeed);
  if (Game.Map[PixToMap(x + GridCellSize), PixToMap(y + GridCellSize)].Back = tSand) and
    (Game.Map[PixToMap(x + GridCellSize - 1), PixToMap(y + GridCellSize - 1)].Back = tSand) then
      Result := Result shr 1;
end; { GetSpeed }

procedure THero.Draw;
begin
  if Ghostly then
    RecoveryTime := 127;
  inherited;
end; { Draw }

{ TDragon }

function TDragon.GetPhase(Next: Boolean): Word;
//const
//  c: array [0..7] of Byte = (1, 0, 2, 3, 3, 2, 0, 1);
begin
//  with Game do
//    Result := ifop(State = ssNormal, c[Counter shr 1 and 7],
//      inherited GetPhase(Next));
  with Game do
    Result := ifop(State = ssNormal,
      Ord(Hero.x >= x) shl 1 + Ord(abs(Hero.y - y) < abs(Hero.x - x)),
      inherited GetPhase(Next));
end; { GetPhase }

procedure TDragon.Live;
var
  i, b: Word;
begin
  if not LevelHint or (Game.GameState <> gsCatchKey) then Exit;
  b := 0;
  with Game.SpriteEngine do
    for i := 0 to Sprites.Count - 1 do
      with TActiveSprite(Sprites[i]) do
        if SpriteType = sBox then
          if x = Self.x then
            Inc(b, ifop(y = Self.y - MapCellSize, 1, ifop(y = Self.y + MapCellSize, 4, 0)))
          else if y = Self.y then
            Inc(b, ifop(x = Self.x - MapCellSize, 2, ifop(x = Self.x + MapCellSize, 3, 0)));
  if b <> 1 + 2 + 3 + 4 then Exit;
  LevelHint := False;
  Game.Hero.Ghostly := True;
end; { Live }

{ TFireball }

procedure TFireball.Live;
begin
  with Game do
    if (State <> ssSleeping) and not isShooting and (GameState = gsCatchKey) then
      if (dX <> 0) and (abs(Hero.y - y) <= GridCellSize) and (sgn(Hero.x - x) = dX) or
        (dY <> 0) and (abs(Hero.x - x) <= GridCellSize) and (sgn(Hero.y - y) = dY) then
          Attack(dX, dY);
end; { Live }

function TFireball.GetPhase(Next: Boolean): Word;
begin
  Result := inherited GetPhase(Next);
    if (State = ssNormal) and (Game.GameState = gsCatchKey) then
      Result := Result or 4;
end; { GetPhase }

procedure TFireball.SetState(const Value: TSpriteState);
begin
  inherited;
  if LevelHint and (Value = ssSwimming) and (Game.Hero.InBasket(tBridgeVertical)) then
  begin
    LevelHint := False;
    DoHint;
    Kill;
  end;
end; { SetState }

class procedure TFireball.DoHint;
begin
  Game.ReplaceTiles(tLava, tBasalt, mlBack);
end; { DoHint }

{ TMedusa }

procedure TMedusa.Live;
begin
  with Game do
    if (State <> ssSleeping) and not isShooting and
      ((Hero.x = x) or (Hero.y = y)) then
        Attack(sgn(Hero.x - x), sgn(Hero.y - y));
end; { Live }

function TMedusa.GetPhase(Next: Boolean): Word;
begin
  with Game do
    Result := Ord((State <> ssSleeping) and
      ((abs(Hero.x - x) <= GridCellSize) or (abs(Hero.y - y) <= GridCellSize)));
end; { GetPhase }

{ TBullet }

constructor TBullet.Create(sType: TSpriteType; aShooter: TActiveSprite;
  adx, ady: TDelta; CheckPath: Boolean);
begin
  with aShooter do
    inherited Create(sType, 0, x, y, adx, ady, False);
  Shooter := aShooter;
  if CheckPath then
    if CanHitTarget then
      Game.Hero.State := ssSleeping
    else
    begin
      Game.SpriteEngine.RemoveSprite(Self);
      Exit;
    end;
  Shooter.isShooting := True;
end; { Create }

procedure TBullet.Live;
begin
  if Moving or Pushed(dX, dY) then Exit;

  // if bullet cannot fly further
  with CellInfo, Game do
    if Shooter = Hero then
    begin
      // if Hero shoots and hits in centre of Sprite (not box) he hurts it
      if Sprite[2] <> nil then
        with Sprite[2] do
          if not (SpriteType in [sBox, sHero]) then
            Hurt(dX, dY);
      if ([Cell[0].Item, Cell[1].Item] <= ChestItems) and
        (Cell[0].Weight * Cell[1].Weight <> 0) then
          CheckHidden(True);
    end
    // if bullet just touches Hero he dies
    else if (Sprite[0] = Hero) or (Sprite[1] = Hero) then
      GameState := gsKillHero;

  Kill;
  Shooter.isShooting := False;
end; { Live }

function TBullet.CanStepOn(const Info: TCellInfo): Boolean;
var
  Items: TTiles;
begin
  Items := BulletPathItems;
  with Info do
  begin
    if Cell[0].Weight + Cell[1].Weight <> 0 then
      Items := Items - ChestItems;
    Result := ([Cell[0].Back, Cell[1].Back] <= BulletPathBacks) and
      ([Cell[0].Item, Cell[1].Item] <= Items);
  end;
end; { CanStepOn }

function TBullet.CanHitTarget: Boolean;
var
  Xold, Yold: TPixCoord;
begin
  Xold := X;
  Yold := Y;
  while not HurdleInFront do
  begin
    Inc(X, GridToPix(dX));
    Inc(Y, GridToPix(dY));
  end;
  Result := (CellInfo.Sprite[0] = Game.Hero) or (CellInfo.Sprite[1] = Game.Hero);
  X := Xold;
  Y := Yold;
end; { CanHitTarget }

function TBullet.GetSpeed: TSpeed;
begin
  Result := ifop(SpriteType = sFire, 8, 16);
end; { GetSpeed }

{ TBox }

function TBox.CanStepOn(const Info: TCellInfo): Boolean;
begin
  with Info do
    Result := ([Cell[0].Back, Cell[1].Back] <= BoxPathBacks) and
      ([Cell[0].Item, Cell[1].Item] <= AnyPathItems);
end; { CanStepOn }

procedure TBox.Live;
var
  i: Word;
begin
  with Game, SpriteEngine do
    if LevelHint and (GameState = gsCatchKey) and (LevelStatus <> lsDone) and
      not Moving then
      begin
        for i := 0 to Sprites.Count - 1 do
          if (Sprites[i] is TBox) then
            with TBox(Sprites[i]), Map[PixToMap(x), PixToMap(y)] do
              if LevelHint and
                (not InMCell or (TSpriteType(Sprite shr 4 ) <> sBox)) then Exit;

        Hero.Ghostly := True;

        for i := 0 to Sprites.Count - 1 do
          if (Sprites[i] is TBox) then
            LevelHint := False;
      end;
  if not Moving and (State = ssSleeping) and not Pushed(dX, dY) then
    State := ssNormal;
end; { Live }

{ TShip }

constructor TShip.Create(aSailor: TActiveSprite; aX, aY: TPixCoord);
begin
  inherited Create(sRaft, 0, aX, aY);
  State := ssSwimming;
  Sailor := aSailor;
  Game.SpriteEngine.SortSprites;
end; { Create }

procedure TShip.Live;
begin
end; { Live }

function TShip.GetPhase(Next: Boolean): Word;
begin
  Result := 0;
end; { GetPhase }

{ TSwingingSprite }

procedure TSwingingSprite.Live;
begin
  if (State <> ssSleeping) and not Moving and not Pushed(dX, dY) then
    Pushed(-dX, -dY);
end; { Live }

{ TCuckold }

function TCuckold.GetPhase(Next: Boolean): Word;
begin
  Result := ifop(State = ssNormal, (AnimPhase + Ord(Next)) and 3,
    inherited GetPhase(Next));
end; { GetPhase }

procedure TCuckold.Live;
begin
  with Game do
    if (State <> ssSleeping) and not isShooting and InGCell then
    begin
      if abs(Hero.x + sgn(Hero.vX) - x) < GridCellSize then
        Attack(0, sgn(Hero.y - y))
      else if abs(Hero.y + sgn(Hero.vY)- y) < GridCellSize then
        Attack(sgn(Hero.x - x), 0);
    end;
  inherited;
end; { Live }

{ TRoamingSprite }

function TRoamingSprite.HeroTouched(adX, adY: TDistance): Boolean;
begin
  Result := (adX <= GridCellSize) and (adY <= GridCellSize);
  if Result then
    Game.GameState := gsKillHero;
end; { HeroTouched }

procedure TRoamingSprite.Live;
var
  d: TDelta;
begin
  if HeroTouched(abs(Game.Hero.x - x), abs(Game.Hero.y - y)) then Exit;

  d := (Ord(x < Game.Hero.x) * 2 - 1) * (Ord(y < Game.Hero.y) * 2 - 1);
  if not Pushed(dX, dY) then
    if not Pushed(-dY * d, dX * d) and not Pushed(-dX, -dY) then
      Pushed(dY * d, -dX * d);
end; { Live }

procedure TRoamingSprite.GetItemsInFront(var Info: TCellInfo);
var
  i: Byte;
begin
  inherited;
  if SpriteType = sStony then Exit;
  for i := 0 to 2 do
    if Info.Sprite[i] = Game.Hero then
      Info.Sprite[i] := nil;
end; { GetItemsInFront }

{ TSkull }

function TSkull.GetPhase(Next: Boolean): Word;
begin
  Result := ifop((State = ssNormal) and (Game.GameState = gsCatchKey),
    ifop(Next, ifop(AnimPhase > 3, 1, AnimPhase + 1), 1),
    inherited GetPhase(Next));
end; { GetPhase }

procedure TSkull.Live;
var
  adx, ady: TDelta;
begin
  if (State = ssSleeping) or (Game.GameState <> gsCatchKey) then Exit;

  if not Moving then
  begin
    adx := sgn(Game.Hero.x - x);
    adx := ifop(adx <> 0, adx, ifop(dX <> 0, dX, 1));
    ady := sgn(Game.Hero.y - y);
    ady := ifop(ady <> 0, ady, ifop(dY <> 0, dY, 1));
    if (abs(Game.Hero.x - x) <= GridToPix(1)) then
      if not Pushed(0, ady) then
        if not Pushed(adx, 0) then
          Pushed(-adx, 0);
    if (abs(Game.Hero.y - y) <= GridToPix(1)) then
      if not Pushed(adx, 0) then
        if not Pushed(0, ady) then
          Pushed(0, -ady);
  end;
  inherited;
end; { Live }

function TSkull.HeroTouched(adX, adY: TDistance): Boolean;
begin
  if Game.GameState = gsCatchKey then
    Result := inherited HeroTouched(adX, adY)
  else
    Result := False;
end; { HeroTouched }

{ TRoller }

function TRoller.GetPhase(Next: Boolean): Word;
begin
  Result := inherited GetPhase(Next);
  if State = ssSleeping then
    Result := Result or 16;
end; { GetPhase }

procedure TRoller.Live;
begin
  with Game do
    if not Moving then
      if State = ssSleeping then
        State := ifop(HurdleInFront, ssNormal, ssSleeping)
      else if abs(Hero.y - y) < GridCellSize then
        State := ifop(Pushed(sgn(Hero.x - x), 0), ssSleeping, ssNormal)
      else if abs(Hero.x - x) < GridCellSize then
        Pushed(0, sgn(Hero.y - y));
  inherited;
end; { Live }

{ TSleepy }

constructor TSleepy.Create(aX, aY: TPixCoord; adX: TDelta = 0; adY: TDelta = 0;
  aLevelHint: Boolean = False);
begin
  inherited Create(sSleepy, 3, aX, aY, adX, adY, aLevelHint);
  if LevelHint and (adX <> 0) then
    Touched := 10;
end; { Create }

procedure TSleepy.Live;
begin
  if (State <> ssSleeping) and not Moving then
    if abs(Game.Hero.x - x) <= 0 then
      Pushed(0, sgn(Game.Hero.y - y))
    else if abs(Game.Hero.y - y) <= 0 then
      Pushed(sgn(Game.Hero.x - x), 0);
  inherited;
end; { Live }

function TSleepy.HeroTouched(adX, adY: TDistance): Boolean;
begin
  Result := (adX <= MapCellSize) and (adY <= GridCellSize)
    or (adX <= GridCellSize) and (adY <= MapCellSize);
  if not Result then Exit;
  vX := 0;
  vY := 0;
  State := ssSleeping;
end; { HeroTouched }

function TSleepy.GetPhase(Next: Boolean): Word;
begin
  if PauseTime > 0 then
    Result := inherited GetPhase(Next)
  else
    Result := Byte(DeltaToIndex(dX, dY) shl 2) or ifop(State = ssSleeping,
      Game.MovePhase shr 2 and $3 or $10, Game.MovePhase {shr 1} and $3);
end; { GetPhase }

procedure TSleepy.TouchMe;
begin
  if LevelHint and (Touched > 0) then
  begin
    Dec(Touched);
    if Touched > 0 then Exit;
    Game.Hero.Ghostly := True;
    LevelHint := False;
  end;
end; { TouchMe }

function TSleepy.GetSpeed: TSpeed;
begin
  Result := ifop(State = ssSleeping, 0, inherited GetSpeed);
end; { GetSpeed }

procedure TSleepy.Hurt(adx, ady: TDelta);
begin
  if State <> ssSleeping then
    inherited Hurt(adx, ady);
end; { Hurt }

procedure TSleepy.SetState(const Value: TSpriteState);
begin
  inherited;
  if LevelHint and (Value = ssSwimming) then
  begin
    LevelHint := False;
    DoHint;
    Kill;
  end;
end; { SetState }

class procedure TSleepy.DoHint;
begin
  Game.ReplaceTiles(tWater, tIce, mlBack);
end; { DoHint }

{ TStony }

function TStony.HeroTouched(adX, adY: TDistance): Boolean;
begin
  Result := (adX = 0) or (adX <= GridCellSize) and (adY <= MapCellSize) or
    (adY <= GridCellSize) and (adX <= MapCellSize * 2);
  if (adX = 0) and (Game.MovePhase and $3 = 0) then
    Pushed(0, sgn(Game.Hero.Y - Y));
end; { HeroTouched }

function TStony.GetSpeed: TSpeed;
begin
  Result := ifop(State = ssSleeping, 0, inherited GetSpeed shr 1);
  if (Result > 0) and (Game.Hero.x = x) and (dY = sgn(Game.Hero.y - y)) then
  begin
    Result := Result shl 1;
    if (dX <> 0) and (x mod Result <> 0) or (dY <> 0) and (y mod Result <> 0) then
      Result := Result shr 1;
  end; { GetSpeed }
end;

function TStony.HurdleInFront: Boolean;
begin
  Result := inherited HurdleInFront;
  with CellInfo do
    if CanStepOn(CellInfo) and (Sprite[2] = Game.Hero) then
      Result := not Sprite[2].Pushed(dX, dY);
end; { HurdleInFront }

end.
