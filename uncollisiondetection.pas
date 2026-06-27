unit unCollisionDetection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uncollisiontypes, unHelperInterfaces, Types;

type
  { TCollisionDetection }

  { TCircleCollisionResult }

  TCircleCollisionResult = class(TInterfacedObject, ICircleCollisionResult)
  private
    FCircleId1, FCircleId2: integer;
    FdCircle1XAtHit: double;
    FdCircle1YAtHit: double;
    FdCircle2XAtHit: double;
    FdCircle2YAtHit: double;
    FdHitTime: double;
    function GetCircleId1: integer;
    function GetCircleId2: integer;
    function GetHitTime: double;
    function GetCircle1XAtHit: double;
    function GetCircle1YAtHit: double;
    function GetCircle2XAtHit: double;
    function GetCircle2YAtHit: double;
  public
    constructor Create(const iCircleID1, iCircleID2: integer;
      const dHitTime: double; const dX1AtHit, dY1AtHit, dX2AtHit, dY2AtHit: double);

  end;

  TCollisionDetection = class
  public
    class procedure DetectEdgeHits(const APathPart: IPathPart;
      var dEarliestHitTime: double; var EdgeHit: TEdgeHit);

    class function DetectStationaryCircleHit(const APathPart1: IPathPart;
      const APathPart2: IPathPart): ICircleCollisionResult;

    class function DetectMovingCircleHit(const APathPart1: IPathPart;
      const APathPart2: IPathPart): ICircleCollisionResult;

    class function CalculateBounceAfterHittingCircle(const APathPart: IPathPart;
      const dX, dY: double; const ATargetPathPart: IPathPart;
      const dHitTime: double): TBounceResult;

    class function DetectPocketed(const APathPart1: IPathPart;
      const ptPocket :TPointF): ICircleCollisionResult;
  end;

{ TCircleCollisionResult }



implementation

uses
  uncirclephysicsconstants, unCirclePhysics, Matrix, unCircleUtils,
  unPathPartImplementation, unOtherCircles, Forms, Math;

procedure LogMessage(const sMessage: string);
var
  intfLogger: IBasicLogger;
begin
  if supports(Application.MainForm, IBasicLogger, intfLogger) then
    intfLogger.LogMessage(sMessage);
end;

{ ----------------------------------------------------------------------------
  Analytic root finders for the two-moving-circle collision solve.

  While both discs move, the relative position is  A + B t + C t^2  (constant
  relative acceleration), so |relative position| = sumRadii is a QUARTIC in t.
  We solve it exactly: bracket the first downcrossing using the quartic's
  critical points (roots of its cubic derivative) then bisect. Ported from the
  JavaScript engine (roots.js / events.js).
  ---------------------------------------------------------------------------- }

const
  ROOT_EPS = 1e-12;
  MC_TIME_EPS = 1e-9;
  MC_CONTACT_EPS = 1e-4; // metres-equivalent: treat as already-touching

// Real cube root (Power() can't take a negative base with a fractional exponent).
function SignedCbrt(const x: double): double;
begin
  if x < 0 then
    Result := -Power(-x, 1.0 / 3.0)
  else
    Result := Power(x, 1.0 / 3.0);
end;

// Real roots of a t^2 + b t + c (0..2), written into r[]; count is the result.
function QuadRealRoots(const a, b, c: double; var r: array of double): integer;
var
  disc, s: double;
begin
  if Abs(a) < ROOT_EPS then
  begin
    if Abs(b) < ROOT_EPS then
      Result := 0
    else
    begin
      r[0] := -c / b;
      Result := 1;
    end;
    Exit;
  end;
  disc := (b * b) - (4 * a * c);
  if disc < 0 then
  begin
    Result := 0;
    Exit;
  end;
  s := Sqrt(disc);
  r[0] := (-b - s) / (2 * a);
  r[1] := (-b + s) / (2 * a);
  Result := 2;
end;

// Real roots of a cubic a t^3 + b t^2 + c t + d (Cardano; trig form for 3 real roots).
function CubicRealRoots(const a, b, c, d: double; var r: array of double): integer;
var
  p, q, rr, bigP, bigQ, shift, disc, s, u, m, arg, th: double;
  k: integer;
begin
  if Abs(a) < ROOT_EPS then
  begin
    Result := QuadRealRoots(b, c, d, r);
    Exit;
  end;
  p := b / a;
  q := c / a;
  rr := d / a;
  // depress: t = x - p/3  ->  x^3 + bigP x + bigQ
  bigP := q - (p * p) / 3;
  bigQ := (2 * p * p * p) / 27 - (p * q) / 3 + rr;
  shift := -p / 3;
  disc := (bigQ * bigQ) / 4 + (bigP * bigP * bigP) / 27;
  if disc > ROOT_EPS then
  begin
    s := Sqrt(disc);
    r[0] := SignedCbrt(-bigQ / 2 + s) + SignedCbrt(-bigQ / 2 - s) + shift;
    Result := 1;
  end
  else if disc < -ROOT_EPS then
  begin
    m := 2 * Sqrt(-bigP / 3);
    arg := (3 * bigQ) / (bigP * m);
    if arg > 1 then arg := 1;
    if arg < -1 then arg := -1;
    th := ArcCos(arg) / 3;
    for k := 0 to 2 do
      r[k] := m * Cos(th - (2 * Pi * k) / 3) + shift;
    Result := 3;
  end
  else
  begin
    u := SignedCbrt(-bigQ / 2);
    r[0] := 2 * u + shift;
    r[1] := -u + shift;
    Result := 2;
  end;
end;

function QuarticEval(const k4, k3, k2, k1, k0, t: double): double;
begin
  Result := ((((k4 * t) + k3) * t + k2) * t + k1) * t + k0;
end;

// First t in (lo,hi] where the quartic crosses to <= 0 (assuming q(lo) > 0). Brackets via
// the quartic's critical points (cubic q'=0) then bisects the first sign-changing segment.
function FirstQuarticRoot(const k4, k3, k2, k1, k0, lo, hi: double;
  out tHit: double): boolean;
var
  crit: array[0..2] of double;
  bps: array[0..3] of double;
  nCrit, nbp, i, j: integer;
  aSeg, bp, x0, x1, mid, tmp: double;
begin
  Result := False;
  tHit := 0;
  if hi <= lo then Exit;
  if QuarticEval(k4, k3, k2, k1, k0, lo) <= 0 then
  begin
    tHit := lo;
    Result := True;
    Exit;
  end;
  // critical points: q'(t) = 4k4 t^3 + 3k3 t^2 + 2k2 t + k1
  nCrit := CubicRealRoots(4 * k4, 3 * k3, 2 * k2, k1, crit);
  nbp := 0;
  for i := 0 to nCrit - 1 do
    if (crit[i] > lo) and (crit[i] < hi) then
    begin
      bps[nbp] := crit[i];
      Inc(nbp);
    end;
  // ascending sort of the in-range critical points
  for i := 0 to nbp - 2 do
    for j := 0 to nbp - 2 - i do
      if bps[j] > bps[j + 1] then
      begin
        tmp := bps[j];
        bps[j] := bps[j + 1];
        bps[j + 1] := tmp;
      end;
  bps[nbp] := hi;
  Inc(nbp);

  aSeg := lo;
  for i := 0 to nbp - 1 do
  begin
    bp := bps[i];
    if QuarticEval(k4, k3, k2, k1, k0, bp) <= 0 then
    begin
      x0 := aSeg;
      x1 := bp;
      while (x1 - x0) > 1e-10 do
      begin
        mid := 0.5 * (x0 + x1);
        if QuarticEval(k4, k3, k2, k1, k0, mid) <= 0 then
          x1 := mid
        else
          x0 := mid;
      end;
      tHit := 0.5 * (x0 + x1);
      Result := True;
      Exit;
    end;
    aSeg := bp;
  end;
end;

// First contact time of relative motion (A + B t + C t^2) reaching |.| = R, within (lo,hi].
function FirstContactTime(const ax, ay, bx, by, cx, cy, R, lo, hi: double;
  out tHit: double): boolean;
var
  k4, k3, k2, k1, k0: double;
begin
  Result := False;
  tHit := 0;
  if hi <= lo + MC_TIME_EPS then Exit;
  k4 := (cx * cx) + (cy * cy);
  k3 := 2 * ((bx * cx) + (by * cy));
  k2 := ((bx * bx) + (by * by)) + 2 * ((ax * cx) + (ay * cy));
  k1 := 2 * ((ax * bx) + (ay * by));
  k0 := ((ax * ax) + (ay * ay)) - (R * R);
  if FirstQuarticRoot(k4, k3, k2, k1, k0, lo, hi, tHit) then
    Result := (tHit > MC_TIME_EPS);
end;




{ TCircleCollisionResult }

function TCircleCollisionResult.GetCircleId1: integer;
begin
  Result := FCircleId1;
end;

function TCircleCollisionResult.GetCircleId2: integer;
begin
  Result := FCircleId2;
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

constructor TCircleCollisionResult.Create(const iCircleId1, iCircleId2: integer;
  const dHitTime: double; const dX1AtHit, dY1AtHit, dX2AtHit, dY2AtHit: double);
begin
  FCircleId1 := iCircleId1;
  FCircleId2 := iCircleId2;
  FdHitTime := dHitTime;
  FdCircle1XAtHit := dX1AtHit;
  FdCircle1YAtHit := dY1AtHit;
  FdCircle2XAtHit := dX2AtHit;
  FdCircle2YAtHit := dY2AtHit;
end;


{ TCollisionDetection }

class procedure TCollisionDetection.DetectEdgeHits(const APathPart: IPathPart;
  var dEarliestHitTime: double; var EdgeHit: TEdgeHit);
var
  dXAtStop, dYAtStop, dDeplacement, dHitTime: double;
  AVector: IBasicVector;
  dRadius: double;
begin
  AVector := APathPart.Vector;
  dRadius := APathPart.Circle.Radius;
  dXAtStop := AVector.GetXAtStop;
  dYAtStop := AVector.GetYAtStop;

  if (dXAtStop <= dRadius) then
  begin
    dDeplacement := AVector.Origin.X - dRadius;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehLeft;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dYAtStop <= dRadius) then
  begin
    dDeplacement := AVector.Origin.Y - dRadius;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehTop;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dXAtStop >= (BOARD_WIDTH - dRadius)) then
  begin
    dDeplacement := BOARD_WIDTH - dRadius - AVector.Origin.X;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehRight;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dYAtStop >= (BOARD_HEIGHT - dRadius)) then
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
  const APathPart1: IPathPart; const APathPart2: IPathPart): ICircleCollisionResult;
var
  dDistanceBetween2Centers, dSumRadii, dDistanceBewteen2Circles,
  dDotProduct_D, dyDistanceToColissionSquared_F, dXDiffereneAtCollision_T,
  dHitTime, dXDistanceToCollision_distance, dActualDistanceToCollision: double;
  ACircle1Vector, ACircle2Vector: IBasicVector;
  AThisVector, AVectorBetween2Centers: I2DVector;
  NormalizedVector_N: I2DVector;
  dXCircleHit, dYCircleHit: double;
  ACircle1, aCircle2: ICircle;
  iCircleId1, iCircleId2: integer;
begin
  Result := nil;

  ACircle1 := APathPart1.Circle;
  aCircle2 := APathPart2.Circle;
  ACircle1Vector := APathPart1.Vector;
  ACircle2Vector := APathPart2.Vector;

  iCircleId1 := TCircleUtils.GetCircleId(ACircle1);
  iCircleId2 := TCircleUtils.GetCircleId(ACircle2);


  // check if we travel far enough to hit circle
  dDistanceBetween2Centers := ACircle2Vector.Origin.Distance(ACircle1Vector.Origin);
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
              (NormalizedVector_N.Vector.Data[0] *
              dXDistanceToCollision_distance);
            dYCircleHit :=
              (NormalizedVector_N.Vector.Data[1] *
              dXDistanceToCollision_distance);

            // Calculate the time at which the collision occurred
            dActualDistanceToCollision :=
              Sqrt(Sqr(dXCircleHit) + Sqr(dYCircleHit));
            dHitTime :=
              TBasicMotion.GetTimeToDistance(
              ACircle1Vector.InitialVelocity, dActualDistanceToCollision);

            Result := TCircleCollisionResult.Create(iCircleId1, iCircleId2,
              dHitTime, dXCircleHit + ACircle1Vector.Origin.X,
              dYCircleHit + ACircle1Vector.Origin.Y, ACircle2Vector.Origin.X,
              ACircle2Vector.Origin.Y);
          end;
        end;
      end;
    end;
  end;
end;

// Earliest contact time between TWO MOVING circles (each decelerating along its own
// heading), or nil. Solved analytically as a quartic in t, in two windows: [0, T1] while
// both still move (constant relative acceleration), then [T1, T2] with the sooner-stopping
// disc frozen and the other still moving. Mirrors the JavaScript engine's detectPair.
class function TCollisionDetection.DetectMovingCircleHit(const APathPart1: IPathPart;
  const APathPart2: IPathPart): ICircleCollisionResult;
var
  AVector1, AVector2: IBasicVector;
  ACircle1, ACircle2: ICircle;
  R, Ta, Tb, T1, T2: double;
  u1, u2, ang1, ang2: double;
  v1x, v1y, v2x, v2y: double;
  acc1x, acc1y, acc2x, acc2y: double;
  p1x, p1y, p2x, p2y: double;
  dpx, dpy, dist, nx, ny, vn: double;
  Ax, Ay, Bx, By, Cx, Cy, tHit, Pfx, Pfy: double;
  bFirstSooner, found: boolean;
  iId1, iId2: integer;
begin
  Result := nil;

  AVector1 := APathPart1.Vector;
  AVector2 := APathPart2.Vector;
  ACircle1 := APathPart1.Circle;
  ACircle2 := APathPart2.Circle;

  R := ACircle1.Radius + ACircle2.Radius;
  u1 := AVector1.InitialVelocity;
  u2 := AVector2.InitialVelocity;
  if (u1 <= 0) and (u2 <= 0) then Exit; // neither moving

  p1x := AVector1.Origin.X;  p1y := AVector1.Origin.Y;
  p2x := AVector2.Origin.X;  p2y := AVector2.Origin.Y;
  ang1 := AVector1.Angle;    ang2 := AVector2.Angle;

  v1x := u1 * Cos(ang1);  v1y := u1 * Sin(ang1);
  v2x := u2 * Cos(ang2);  v2y := u2 * Sin(ang2);

  // Acceleration vectors. DECELERATION is negative, so these oppose each disc's motion.
  acc1x := DECELERATION * Cos(ang1);  acc1y := DECELERATION * Sin(ang1);
  acc2x := DECELERATION * Cos(ang2);  acc2y := DECELERATION * Sin(ang2);

  iId1 := TCircleUtils.GetCircleId(ACircle1);
  iId2 := TCircleUtils.GetCircleId(ACircle2);

  // Re-collision guard: if already touching, only an event if approaching (a just-resolved
  // pair is separating and must not be re-detected at t ~ 0).
  dpx := p1x - p2x;  dpy := p1y - p2y;
  dist := Sqrt((dpx * dpx) + (dpy * dpy));
  if (dist - R) <= MC_CONTACT_EPS then
  begin
    if dist < ROOT_EPS then Exit; // coincident centres
    nx := dpx / dist;  ny := dpy / dist;
    vn := ((v1x - v2x) * nx) + ((v1y - v2y) * ny);
    if vn < 0 then
      Result := TCircleCollisionResult.Create(iId1, iId2, MC_TIME_EPS,
        p1x, p1y, p2x, p2y);
    Exit;
  end;

  Ta := AVector1.GetTimeToStop;
  Tb := AVector2.GetTimeToStop;
  if Ta <= Tb then
  begin
    T1 := Ta;  T2 := Tb;  bFirstSooner := True;
  end
  else
  begin
    T1 := Tb;  T2 := Ta;  bFirstSooner := False;
  end;

  found := False;
  tHit := -1;

  // Window 1: [0, T1] — both discs moving (constant relative acceleration).
  if T1 > 0 then
  begin
    Ax := p1x - p2x;
    Ay := p1y - p2y;
    Bx := v1x - v2x;
    By := v1y - v2y;
    Cx := 0.5 * (acc1x - acc2x);
    Cy := 0.5 * (acc1y - acc2y);
    found := FirstContactTime(Ax, Ay, Bx, By, Cx, Cy, R, 0, T1, tHit);
  end;

  // Window 2: [T1, T2] — the sooner-stopping disc frozen at its rest point, the other moving.
  if (not found) and (T2 > T1) then
  begin
    if bFirstSooner then
    begin
      Pfx := AVector1.GetXAtTime(T1);  // frozen disc 1
      Pfy := AVector1.GetYAtTime(T1);
      Ax := p2x - Pfx;  Ay := p2y - Pfy;
      Bx := v2x;  By := v2y;
      Cx := 0.5 * acc2x;  Cy := 0.5 * acc2y;
    end
    else
    begin
      Pfx := AVector2.GetXAtTime(T1);  // frozen disc 2
      Pfy := AVector2.GetYAtTime(T1);
      Ax := p1x - Pfx;  Ay := p1y - Pfy;
      Bx := v1x;  By := v1y;
      Cx := 0.5 * acc1x;  Cy := 0.5 * acc1y;
    end;
    found := FirstContactTime(Ax, Ay, Bx, By, Cx, Cy, R, T1, T2, tHit);
  end;

  if found then
    Result := TCircleCollisionResult.Create(iId1, iId2, tHit,
      AVector1.GetXAtTime(tHit), AVector1.GetYAtTime(tHit),
      AVector2.GetXAtTime(tHit), AVector2.GetYAtTime(tHit));
end;

class function TCollisionDetection.CalculateBounceAfterHittingCircle(
  const APathPart: IPathPart; const dX, dY: double; const ATargetPathPart: IPathPart;
  const dHitTime: double): TBounceResult;
var
  AVector, intfTargetCircleVector: IBasicVector;

  n, v1, v2, finalv1, finalv2: Tvector2_double;
  a1, a2, optimizedP: double;
  dFactor: double;
begin

  AVector := APathPart.Vector;
  intfTargetCircleVector := ATargetPathPart.Vector;

  v1 := AVector.GetVelocityVectorAtTime(dHitTime);
  v2 := intfTargetCircleVector.GetVelocityVectorAtTime(dHitTime);

  // First, find the normalized vector n from the center of
  // circle1 to the center of circle2
  n.init(DX - intfTargetCircleVector.Origin.X, DY - intfTargetCircleVector.Origin.Y);
  n.init(
    n.Data[0] / n.length,
    n.Data[1] / n.length);
  // Find the length of the component of each of the movement
  // vectors along n.
  // a1 = v1 . n
  // a2 = v2 . n
  a1 := (v1.Data[0] * n.Data[0]) + (v1.Data[1] * n.Data[1]); //v1.dot(n)
  a2 := (v2.Data[0] * n.Data[0]) + (v2.Data[1] * n.Data[1]); // v2.dot(n);

  // Using the optimized version,
  // optimizedP =  2(a1 - a2)
  //              -----------
  //                m1 + m2
  optimizedP := (2.0 * (a1 - a2)) / (APathPart.Circle.mass +
    ATargetPathPart.Circle.mass);

  // Calculate v1', the new movement vector of circle1
  // v1' = v1 - optimizedP * m2 * n
  dFactor := optimizedP * ATargetPathPart.Circle.Mass;
  finalv1.init(v1.Data[0] - (n.Data[0] * dFactor),
    v1.Data[1] - (n.Data[1] * dFactor));

  // Calculate v1', the new movement vector of circle1
  // v2' = v2 + optimizedP * m1 * n
  dFactor := optimizedP * APathPart.Circle.Mass;
  finalv2.init(v2.Data[0] + (n.Data[0] * dFactor),
    v2.Data[1] + (n.Data[1] * dFactor));

  Result.Vector1 := finalv1;
  Result.Vector2 := finalv2;
end;

class function TCollisionDetection.DetectPocketed(
  const APathPart1: IPathPart; const ptPocket :TPointF): ICircleCollisionResult;
var
  dDistanceBetween2Centers, dSumRadii, dDistanceBewteen2Circles,
  dDotProduct_D, dyDistanceToColissionSquared_F, dXDiffereneAtCollision_T,
  dHitTime, dXDistanceToCollision_distance, dActualDistanceToCollision: double;
  ACircle1Vector, ACircle2Vector: IBasicVector;
  AThisVector, AVectorBetween2Centers: I2DVector;
  NormalizedVector_N: I2DVector;
  dXCircleHit, dYCircleHit: double;
  ACircle1, aCircle2: ICircle;
  iCircleId1, iCircleId2: integer;

  APathPart2 : IPathPart;
begin
  Result := nil;


  //TODO - moving circle should have radius of 0 or 1
  //Pocket radius should be POCKET_RADIUS - realPuckRadius
  ACircle1 := APathPart1.Circle;
  ACircle1Vector := APathPart1.Vector;

  aCircle2 := TBaseCircle.Create(POCKET_RADIUS, 1);
  ACircle2Vector := TBasicVector.Create(ptPocket, 0, 0,0);


  APathPart2 := TPathPart.Create(aCircle2, ACircle2Vector);
  iCircleId1 := TCircleUtils.GetCircleId(ACircle1);
  iCircleId2 := TCircleUtils.GetCircleId(ACircle2);


  // check if we travel far enough to hit circle
  dDistanceBetween2Centers := ACircle2Vector.Origin.Distance(ACircle1Vector.Origin);
  dSumRadii := POCKET_RADIUS -   ACircle1.Radius;
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
              (NormalizedVector_N.Vector.Data[0] *
              dXDistanceToCollision_distance);
            dYCircleHit :=
              (NormalizedVector_N.Vector.Data[1] *
              dXDistanceToCollision_distance);

            // Calculate the time at which the collision occurred
            dActualDistanceToCollision :=
              Sqrt(Sqr(dXCircleHit) + Sqr(dYCircleHit));
            dHitTime :=
              TBasicMotion.GetTimeToDistance(
              ACircle1Vector.InitialVelocity, dActualDistanceToCollision);

            Result := TCircleCollisionResult.Create(iCircleId1, iCircleId2,
              dHitTime, dXCircleHit + ACircle1Vector.Origin.X,
              dYCircleHit + ACircle1Vector.Origin.Y, ACircle2Vector.Origin.X,
              ACircle2Vector.Origin.Y);
          end;
        end;
      end;
    end;
  end;
end;


{

Moving circles.

Find the moving circle with the shortest duration

for this duration check if the rectangle of movement (+radius) interesects with the other (+radius)

if not then this becomes a stationary circle problem.


(WHEN A BALL STOPS RECALCULATE TRAJETORIES!! -- probably not needed)

If they do intersect then subtract the vector with minimum magnitude from the
vector with max magnitude (does this make it static???) and apply the steps
in  DetectStationaryCircleHit.

}

end.
