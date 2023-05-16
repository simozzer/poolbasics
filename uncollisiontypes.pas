unit uncollisiontypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Matrix, unHelperInterfaces;

type
  // Enumeration for describing which objects were hit
  TEdgeHit = (ehNone, ehLeft, ehTop, ehRight, ehBottom, ehCircle);


  // Which edge was hit, and at what time
  TEdgeHitDetail = record
    EdgeHit: TEdgeHit;
    HitTime: double;
    iPathPartId : Integer;
    intfDetails: IInterface;
    // Used to store result (which circle or circle collision results)
  end;

  // Information describing the trajectories (velocities) of 2 circles after collision
  TBounceResult = record
    Vector1: Tvector2_double;
    Vector2: Tvector2_double;
  end;


function EdgeHitToStr(const EdgeHit: TEdgeHit): string;

implementation

function EdgeHitToStr(const EdgeHit: TEdgeHit): string;
begin
  case EdgeHit of
    ehNone: Result := 'None';
    ehLeft: Result := 'Left';
    ehTop: Result := 'Top';
    ehRight: Result := 'Right';
    ehBottom: Result := 'Bottom';
    ehCircle: Result := 'Circle';
  end;
end;

end.
