unit unBallsInMotion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unHelperInterfaces, uncollisiontypes, fgl;

type


  { TTimeslice }

  TTimeslice = class(TInterfacedObject, ITimeslice)
  private
    FdStartTime: double;
    FdEndTime: double;
    FlstCircles: ICirclesList;
  protected
    function GetStartTime: double;
    function GetEndTime: double;
    function GetCircles: ICirclesList;
    procedure SetStartTime(const dStartTime: double);
    procedure SetEndTime(const dEndTime: double);
    property StartTime: double read GetStartTime write SetStartTime;
    property EndTime: double read GetEndTime write SetEndTime;
    property Circles: ICirclesList read GetCircles;

    function ToString(): String; override;
  public
    constructor Create(const ACircles: ICirclesList);
    destructor Destroy; override;
  end;




  { TTimesliceList }

  TTimesliceList = class(TInterfacedObject, ITimesliceList)
  private
    FList: TObject;
  protected
    function getItem(const iIndex: integer): ITimeslice;
    function GetCount: cardinal;
    procedure Clear;
    procedure Add(const intfTimeslice: ITimeslice);
    property Count: cardinal read GetCount;
    property Item[const iIndex: integer]: ITimeslice read GetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TCirclePathCalculator }

  TCirclePathCalculator = class(TInterfacedObject, IPathPlotter, IBasicLoggerClient)
  private
    FlstCircles: ICirclesList;
    FintfLogger: IBasicLogger;

    FlstTimeslices: ITimesliceList;
    function FindCircleById(const cCircleId: cardinal;
      const ACirclesList: ICirclesList): ICircle;

    function GetMaxTimeFromCirclesList(const ACircles : ICirclesList): Double;
  protected
    function GetMovingCirclesAtTime(const ACirclesList: ICirclesList;
      const dTime: double): ICirclesList;
    function GetStationaryCirclesAtTime(const ACirclesList: ICirclesList;
      const dTime: double): ICirclesList;
    function GetNextCollisionFromTime(const ACirclesList: ICirclesList;
      const dTime: double): TEdgeHitDetail;

    function GetCirclesStateAtTime(const ACirclesList: ICirclesList;
      const dTime: double): ICirclesList;
    function GetTimeslices: ITimesliceList;
    procedure LogMessage(const sMessage : String);
    procedure SetLogger(const intfLogger : IBasicLogger);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCircle(const ACircle: ICircle);
    procedure Clear;
    procedure GainThePlot;
    property Timeslices: ITimesliceList read GetTimeslices;
  end;

implementation

uses
  unCollisionDetection, ComObj, unOtherCircles, unCirclePhysics, Types;

type
  TInternalTimesliceList = specialize TFPGInterfacedObjectList<ITimeslice>;

{ TTimesliceList }

function TTimesliceList.getItem(const iIndex: integer): ITimeslice;
begin
  Result := TInternalTimesliceList(FList)[iIndex];
end;

function TTimesliceList.GetCount: cardinal;
begin
  Result := TInternalTimesliceList(FList).Count;
end;

procedure TTimesliceList.Clear;
begin
  TInternalTimesliceList(FList).Clear;
end;

procedure TTimesliceList.Add(const intfTimeslice: ITimeslice);
begin
  TInternalTimesliceList(FList).Add(intfTimeslice);
end;

constructor TTimesliceList.Create;
begin
  FList := TInternalTimesliceList.Create;
end;

destructor TTimesliceList.Destroy;
begin
  TInternalTimesliceList(FList).Clear;
  FList.Free;
  inherited Destroy;
end;

{ TTimeslice }

function TTimeslice.GetStartTime: double;
begin
  Result := FdStartTime;
end;

function TTimeslice.GetEndTime: double;
begin
  Result := FdEndTime;
end;

function TTimeslice.GetCircles: ICirclesList;
begin
  Result := FlstCircles;
end;

procedure TTimeslice.SetStartTime(const dStartTime: double);
begin
  FdStartTime := dStartTime;
end;

procedure TTimeslice.SetEndTime(const dEndTime: double);
begin
  FdEndTime := dEndTime;
end;

function TTimeslice.ToString: String;
var
  sl : TStrings;
  I : Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add(Format('Start Time: %f, End Time: %f',[FdStartTime, FdEndTime]));
    for i := 0 to pred(FlstCircles.Count) do
    begin
      sl.Add('    (' + IntToStr(i) + ')' + FlstCircles[i].ToString());
    end;
    RESULT := sl.CommaText;
  finally
    sl.Free;
  end;
end;

constructor TTimeslice.Create(const ACircles: ICirclesList);
begin
  FlstCircles := ACircles;
  FdEndTime := 0;
  FdStartTime := 0;
end;

destructor TTimeslice.Destroy;
begin
  FlstCircles := nil;
  inherited Destroy;
end;

{ TCirclePathCalculator }

function TCirclePathCalculator.FindCircleById(const cCircleId: cardinal;
  const ACirclesList: ICirclesList): ICircle;
var
  i: integer;
  intfIdentity: IIdentity;
  intfCircle: ICircle;
begin
  Result := nil;
  for i := 0 to pred(ACirclesList.Count) do
  begin
    if supports(ACirclesList[i], IIdentity, intfIdentity) then
    begin
      if intfIdentity.Id = cCircleId then
      begin
        if supports(intfIdentity, ICircle, intfCircle) then
        begin
          Result := intfCircle;
          exit;
        end
        else
        begin
          raise Exception.Create('Could not obtain ICircle');
        end;
      end;
    end
    else
      raise Exception.Create('Could not obtain IObjectWithVector');
  end;
end;

function TCirclePathCalculator.GetMaxTimeFromCirclesList(
  const ACircles: ICirclesList): Double;
var
  dTime : Double;
    i: integer;
  intfObjectWithVector: IObjectWithVector;
begin
  RESULT := 0;
    for i := 0 to pred(ACircles.Count) do
  begin
    if supports(ACircles[i], IObjectWithVector, intfObjectWithVector) then
    begin
      dTime := TBasicMotion.GetTimeToStop(intfObjectWithVector.Vector.InitialVelocity);
      if (dTime > RESULT) then RESULT := dTime;
    end
    else
      raise Exception.Create('Could not obtain IObjectWithVector');
  end;

end;

function TCirclePathCalculator.GetMovingCirclesAtTime(const ACirclesList: ICirclesList;
  const dTime: double): ICirclesList;
var
  i: integer;
  intfObjectWithVector: IObjectWithVector;
begin
  Result := TCirclesList.Create;
  for i := 0 to pred(ACirclesList.Count) do
  begin
    if supports(ACirclesList[i], IObjectWithVector, intfObjectWithVector) then
    begin
      if intfObjectWithVector.Vector.GetVelocityAtTime(dTime) > 0 then
        Result.add(ACirclesList[i]);
    end
    else
      raise Exception.Create('Could not obtain IObjectWithVector');
  end;
end;

function TCirclePathCalculator.GetStationaryCirclesAtTime(
  const ACirclesList: ICirclesList; const dTime: double): ICirclesList;
var
  i: integer;
  intfObjectWithVector: IObjectWithVector;
begin
  Result := TCirclesList.Create;
  for i := 0 to pred(ACirclesList.Count) do
  begin
    if supports(ACirclesList[i], IObjectWithVector, intfObjectWithVector) then
    begin
      if intfObjectWithVector.Vector.GetVelocityAtTime(dTime) <= 0 then
        Result.add(ACirclesList[i]);
    end
    else
      raise Exception.Create('Could not obtain IObjectWithVector');
  end;
end;

function TCirclePathCalculator.GetNextCollisionFromTime(
  const ACirclesList: ICirclesList; const dTime: double): TEdgeHitDetail;
var
  intfMovingCircles: ICirclesList;
  intfStationaryCircles: ICirclesList;
  i, j: integer;
  dEarliestHitTime: double;
  dEdgeHitTime: double;
  intfCircle: ICircle;
  intfObjectWithVector: IObjectWithVector;
  AEdgeHit: TEdgeHit;
  intfEdgeHitCirle: ICircle;
  intfCircleCollisionResult: ICircleCollisionResult;
  intfStoreCircleCollisionResult: ICircleCollisionResult;
begin
  Result.EdgeHit := ehNone;
  Result.HitTime := 0;
  AEdgeHit:= ehNone;

  // for each moving circle find the next edge hit and return the detail from the 1st collision
  intfMovingCircles := GetMovingCirclesAtTime(ACirclesList, dTime);
  intfStationaryCircles := GetStationaryCirclesAtTime(ACirclesList, dTime);
  dEarliestHitTime := -1;
  for i := 0 to pred(intfMovingCircles.Count) do
  begin

    intfCircle := intfMovingCircles[i];
    if supports(intfCircle, IObjectWithVector, intfObjectWithVector) then
    begin
      // Test if circle hits edge of game board
      dEdgeHitTime := dEarliestHitTime;
      AEdgeHit := ehNone;
      TCollisionDetection.DetectEdgeHits(intfObjectWithVector.Vector,
        intfCircle.Radius, dEdgeHitTime, AEdgeHit);
      if (AEdgeHit <> ehNone) and ((dEarliestHitTime < 0) or
        (dEdgeHitTime < dEarliestHitTime)) then
      begin
        intfEdgeHitCirle := intfCircle;
        dEarliestHitTime := dEdgeHitTime;
      end;

      // Test if circle hits a stationary circle
      {*
      for j := 0 to pred(intfStationaryCircles.Count) do
      begin
        intfCircleCollisionResult :=
          TCollisionDetection.DetectStationaryCircleHit(intfCircle,
          intfStationaryCircles[j]);
        if supports(intfCircleCollisionResult, ICircleCollisionResult) and
          ((intfCircleCollisionResult.HitTime < dEarliestHitTime) or
          (dEarliestHitTime < 0)) then
        begin
          // store circle collision result
          AEdgeHit := ehCircle;
          intfStoreCircleCollisionResult := intfCircleCollisionResult;
          dEarliestHitTime := intfCircleCollisionResult.HitTime;
        end;
      end;
      *}

    end
    else
      raise Exception.Create('Could not obtain interface IObjectWithVector');
  end;

  if AEdgeHit = ehCircle then
  begin
    Result.EdgeHit := ehCircle;
    Result.HitTime := intfCircleCollisionResult.HitTime;
    Result.intfDetails := intfStoreCircleCollisionResult;
  end
  else if (AEdgeHit <> ehNone) then
  begin
    Result.EdgeHit := AEdgeHit;
    Result.HitTime := dEarliestHitTime;
    Result.intfDetails := intfEdgeHitCirle;
  end
  else
  begin
    Result.EdgeHit := ehNone;
    Result.HitTime := -1;
    Result.intfDetails := nil;
  end;
end;

function TCirclePathCalculator.GetCirclesStateAtTime(const ACirclesList: ICirclesList;
  const dTime: double): ICirclesList;
var
  i: integer;
  intfSourceObjectWithVector, intfTargetObjectWithVector: IObjectWithVector;
  intfSourceVector, intfTargetVector: IBasicVector;
  intfSourceCircle, intfResultCircle: ICircle;
begin
  Result := TCirclesList.Create;
  for i := 0 to pred(ACirclesList.Count) do
  begin
    intfSourceCircle := ACirclesList[i];


    if supports(intfSourceCircle, IObjectWithVector, intfSourceObjectWithVector) then
    begin
      intfSourceVector := intfSourceObjectWithVector.Vector;
      intfResultCircle := intfSourceObjectWithVector.Clone;
      if not supports(intfResultCircle, IObjectWithVector,
        intfTargetObjectWithVector) then
        raise Exception.Create('could not obtain IObjectWithVector from target')
      else
      begin
        intfTargetVector := intfTargetObjectWithVector.GetBasicVector;
        intfTargetVector.InitialVelocity :=
          intfSourceVector.GetVelocityAtTime(dTime);


      // intfTargetVector.Origin.X := new TPointF(;
       //intfTargetVector.Origin.Y := ;
       intfTargetVector.Origin := TPointF.Create(
             intfSourceVector.GetXAtTime(dTime),
             intfSourceVector.GetYAtTime(dTime)
       );


      end;

      Result.Add(intfResultCircle);
    end
    else
      raise Exception.Create('Could not obtain IObjectWithVector from source');

  end;
end;

function TCirclePathCalculator.GetTimeslices: ITimesliceList;
begin
  Result := FlstTimeslices;
end;

procedure TCirclePathCalculator.LogMessage(const sMessage: String);
begin
  if supports(FintfLogger, IBasicLogger) then
    FintfLogger.LogMessage(sMessage);
end;

procedure TCirclePathCalculator.SetLogger(const intfLogger: IBasicLogger);
begin
  FintfLogger := intfLogger;
end;

constructor TCirclePathCalculator.Create;
begin
  FlstCircles := TCirclesList.Create;
  FlstTimeslices := TTimesliceList.Create;
  FintfLogger := nil;
end;

destructor TCirclePathCalculator.Destroy;
begin
  FlstTimeslices := nil;
  FlstCircles := nil;
  inherited Destroy;
end;

procedure TCirclePathCalculator.AddCircle(const ACircle: ICircle);
begin
  FlstCircles.add(ACircle);
end;

procedure TCirclePathCalculator.Clear;
begin
  FlstCircles.Clear;
end;

{*
  TPath slot: // Begintime, endTime
  TPath slot detail: // start position and velocity of each circle
*}
procedure TCirclePathCalculator.GainThePlot;
var
  dTime, dLastEndTime: double;
  AHitDetail: TEdgeHitDetail;
  CurrentCirclesList: ICirclesList;
  intfTimeslice: ITimeslice;
  intfCircle: ICircle;
  intfCircleCollisionResult: ICircleCollisionResult;
  intfObjectWithVector: IObjectWithVector;
begin
  FlstTimeslices.Clear;

  // Record starting position
  CurrentCirclesList := GetCirclesStateAtTime(FlstCircles, 0);
  dTime := 0;
  dLastEndTime := 0;

  intfTimeslice := TTimeslice.Create(CurrentCirclesList);
  intfTimeslice.StartTime := 0;
  intfTimeslice.EndTime := GetMaxTimeFromCirclesList(CurrentCirclesList);
  //FlstTimeslices.Add(intfTimeslice);

  AHitDetail := GetNextCollisionFromTime(CurrentCirclesList, 0);
  while (AHitDetail.EdgeHit <> ehNone) do
  begin
    // mark the end of the timeslice that caused this collision and move to the next one
    intfTimeslice.EndTime := intfTimeSlice.StartTime + AHitDetail.HitTime;

  //  if (intfTimeslice.StartTime > 0) then
        FlstTimeslices.Add(intfTimeslice);
        LogMessage(intfTimeslice.ToString);


    intfTimeslice := TTimeslice.Create(GetCirclesStateAtTime(
      intfTimeslice.Circles, AHitDetail.HitTime));
    intfTimeslice.StartTime := AHitDetail.HitTime;
    intfTimeslice.EndTime := AHitDetail.HitTime + GetMaxTimeFromCirclesList(intfTimeslice.Circles);


    //FlstTimeslices.add(intfTimeslice);

    if (AHitDetail.EdgeHit <> ehCircle) and
      supports(AHitDetail.intfDetails, ICircle, intfCircle) then
    begin
      if not Supports(intfCircle, IObjectWithVector, intfObjectWithVector) then
        raise Exception.Create('Could not obtain IObjectWithVector');
    end;
    if (AHitDetail.EdgeHit = ehCircle) then
    begin
      if Supports(AHitDetail.intfDetails, ICircleCollisionResult,
        intfCircleCollisionResult) then
      begin

      end
      else
        raise Exception.Create('could not obtain ICircleCollisionResult');
    end;

    // Setup next trajectory
    case AHitDetail.EdgeHit of

      ehLeft, ehRight:
      begin
        intfObjectWithVector.GetBasicVector.ReverseX();
      end;

      ehTop, ehBottom:
      begin
        intfObjectWithVector.GetBasicVector.ReverseY();
      end;

      ehCircle:
      begin
        raise Exception.Create('not yet implemented');
      end;

    end;



    LogMessage(EdgeHitToStr(AHitDetail.EdgeHit) + ' ' +  FloatToStr(AHitDetail.HitTime));
    // go to next change in collision
    AHitDetail := GetNextCollisionFromTime(intfTimeslice.Circles,intfTimeslice.StartTime + AHitDetail.HitTime);
   // AhitDetail.EdgeHit := ehNone; // todo:: remove

  end;

  LogMessage(Format('Timeslice count: %d',[FlstTimeslices.Count]));
end;

end.
