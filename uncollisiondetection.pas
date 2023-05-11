unit unCollisionDetection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uncollisiontypes, unHelperInterfaces;

type
  { TCollisionDetection }

  { TCircleCollisionResult }

  TCircleCollisionResult = class(TInterfacedObject,ICircleCollisionResult)
  private
    FCircle1, FCircle2: ICircle;
    FdCircle1XAtHit: double;
    FdCircle1YAtHit: double;
    FdCircle2XAtHit: double;
    FdCircle2YAtHit: double;
    FdHitTime: double;
    function GetCircle1: ICircle;
    function GetCircle2: ICircle;
    function GetHitTime: double;
    function GetCircle1XAtHit: double;
    function GetCircle1YAtHit: double;
    function GetCircle2XAtHit: double;
    function GetCircle2YAtHit: double;
  public
    constructor Create(const ACircle1, ACircle2: ICircle; const dHitTime: double;
      const dX1AtHit, dY1AtHit, dX2AtHit, dY2AtHit: double);
    property Circle1: ICircle read GetCircle1;
    property Circle2: ICircle read GetCircle2;
    property Circle1XAtHit: double read GetCircle1XAtHit;
    property Circle1YAtHit: double read GetCircle1YAtHit;
    property Circle2XAtHit: double read GetCircle2XAtHit;
    property Circle2YAtHit: double read GetCircle2YAtHit;
    property HitTime: double read GetHitTime;
  end;

  TCollisionDetection = class
  public
    class procedure DetectEdgeHits(const AVector: IBasicVector;
      const dRadius: double; var dEarliestHitTime: double; var EdgeHit: TEdgeHit);

    class function DetectStationaryCircleHit(const ACircle1: ICircle;
      const ACircle2: ICircle): ICircleCollisionResult;
  end;

{ TCircleCollisionResult }



implementation

uses
  uncirclephysicsconstants, unCirclePhysics;

{ TCircleCollisionResult }

function TCircleCollisionResult.GetCircle1: ICircle;
begin
  Result := FCircle1;
end;

function TCircleCollisionResult.GetCircle2: ICircle;
begin
  Result := FCircle2;
end;

function TCircleCollisionResult.GetHitTime: double;
begin
  Result := FdHitTime;
end;

function TCircleCollisionResult.GetCircle1XAtHit: double;
begin
  Result := FdCircle1XAtHit;
end;

function TCircleCollisionResult.GetCircle1YAtHit: double;
begin
  Result := FdCircle1YAtHit;
end;

function TCircleCollisionResult.GetCircle2XAtHit: double;
begin
  Result := FdCircle2XAtHit;
end;

function TCircleCollisionResult.GetCircle2YAtHit: double;
begin
  Result := FdCircle2YAtHit;
end;

constructor TCircleCollisionResult.Create(const ACircle1, ACircle2: ICircle;
  const dHitTime: double; const dX1AtHit, dY1AtHit, dX2AtHit, dY2AtHit: double);
begin
  FCircle1 := ACircle1;
  FCircle2 := ACircle2;
  FdHitTime := dHitTime;
  FdCircle1XAtHit := dX1AtHit;
  FdCircle1YAtHit := dY1AtHit;
  FdCircle2XAtHit := dX2AtHit;
  FdCircle2YAtHit := dY2AtHit;
end;


{ TCollisionDetection }

class procedure TCollisionDetection.DetectEdgeHits(const AVector: IBasicVector;
  const dRadius: double; var dEarliestHitTime: double; var EdgeHit: TEdgeHit);
var
  dMaxDisplacmentXAtStop, dMaxDisplacmentYAtStop, dDeplacement, dHitTime: double;
begin
  dMaxDisplacmentXAtStop := AVector.GetDisplacementXAtStop;
  dMaxDisplacmentYAtStop := AVector.GetDisplacementYAtStop;


  if (dMaxDisplacmentXAtStop <= dRadius) then
  begin
    dDeplacement := AVector.Origin.X - dRadius;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehLeft;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentYAtStop <= dRadius) then
  begin
    dDeplacement := AVector.Origin.Y - dRadius;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehTop;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentXAtStop >= (BOARD_WIDTH - dRadius)) then
  begin
    dDeplacement := BOARD_WIDTH - dRadius - AVector.Origin.X;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehRight;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentYAtStop >= (BOARD_HEIGHT - dRadius)) then
  begin
    dDeplacement := BOARD_HEIGHT - dRadius - AVector.Origin.Y;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehBottom;
      dEarliestHitTime := dHitTime;
    end;
  end;
end;

class function TCollisionDetection.DetectStationaryCircleHit(
  const ACircle1: ICircle; const ACircle2: ICircle): ICircleCollisionResult;
var
  dDistanceBetween2Centers, dSumRadii, dDistanceBewteen2Circles,
  dDotProduct_D, dyDistanceToColissionSquared_F, dXDiffereneAtCollision_T,
  dHitTime, dXDistanceToCollision_distance, dActualDistanceToCollision: double;
  ACircle1VectorAccess, ACircle2VectorAccess: IObjectWithVector;
  ACircle1Vector, ACircle2Vector: IBasicVector;
  AThisVector, AVectorBetween2Centers: I2DVector;
  NormalizedVector_N: I2DVector;
  dXCircleHit, dYCircleHit: double;
begin
  Result := nil;

  if not supports(ACircle1, IObjectWithVector, ACircle1VectorAccess) then
    raise Exception.Create('Could not access IObjectWithVector');

  ACircle1Vector := ACircle1VectorAccess.Vector;

  if not supports(ACircle2, IObjectWithVector, ACircle2VectorAccess) then
    raise Exception.Create('Could not access IObjectWithVector');

  ACircle2Vector := ACircle2VectorAccess.Vector;

  // check if we travel far enough to hit circle
  //TODO: DISTANCE dDistanceBetween2Centers := ACircle2.Distance(ACircle1.CenterX, ACircle1.CenterY);
  dSumRadii := ACircle1.Radius + ACircle2.Radius;
  dDistanceBewteen2Circles := dDistanceBetween2Centers - dSumRadii;
  if (dDistanceBewteen2Circles < TBasicMotion.GetDistanceToStop(
    ACircle1Vector.InitialVelocity)) then
  begin
    AThisVector := T2DVector.CreateWithAngle(dDistanceBewteen2Circles,
      ACircle1Vector.Angle);

    // Get the normalized vector for this ball
    NormalizedVector_N := AThisVector.GetNormalised;

    // Get the vector between the 2 ball centers
    AVectorBetween2Centers :=
          T2DVector.Create(ACircle2Vector.Origin.X - ACircle1Vector.Origin.X,
            ACircle2Vector.Origin.Y - ACircle1Vector.Origin.Y);

    //dDotProduct := AVectorBetween2Centers.Magnitude * cos(-AVectorBetween2Centers.Angle);
    dDotProduct_D := AVectorBetween2Centers.GetDotProduct(NormalizedVector_N);

    // check we're moving towards the target
    if (dDotProduct_D > 0) then
    begin
      // Check that we get close enough for collision
      // double F = (lengthC * lengthC) - (D * D);
      dyDistanceToColissionSquared_F :=
        Sqr(AVectorBetween2Centers.Magnitude) - sqr(dDotProduct_D);
      if (dyDistanceToColissionSquared_F < SQr(dSumRadii)) then
      begin
        // find the distance
        //double T = sumRadiiSquared - F;
        dXDiffereneAtCollision_T :=
          Sqr(dSumRadii) - dyDistanceToColissionSquared_F;
        if (dXDiffereneAtCollision_T >= 0) then
        begin
          //doubleble distance = D - sqrt(T);
          dXDistanceToCollision_distance :=
            dDotProduct_D - Sqrt(dXDiffereneAtCollision_T);
          // check distance to travel is enough for possible collision
          if TBasicMotion.GetDistanceToStop(ACircle1Vector.InitialVelocity) >=
            dXDistanceToCollision_distance then
          begin
            // Set the length so that the circles will just touch.
            dXCircleHit :=
              NormalizedVector_N.Vector.Data[0] *
              dXDistanceToCollision_distance;
            dYCircleHit :=
              NormalizedVector_N.Vector.Data[1] *
              dXDistanceToCollision_distance;

            // Calculate the time at which the collision occurred
            dActualDistanceToCollision :=
              Sqrt(Sqr(dXCircleHit) + Sqr(dYCircleHit));
            dHitTime :=
              TBasicMotion.GetTimeToDistance(
              ACircle1Vector.InitialVelocity, dActualDistanceToCollision);

            Result := TCircleCollisionResult.Create(ACircle1, ACircle2,
              dHitTime, dXCircleHit, dYCircleHit, ACircle2Vector.Origin.X, ACircle2Vector.Origin.Y);
          end;
        end;
      end;
    end;
  end;
end;


end.
