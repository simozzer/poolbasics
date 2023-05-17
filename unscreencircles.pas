unit unScreenCircles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unHelperInterfaces, types, Graphics;

type

  { TScreenCircle }

  TScreenCircle = class(TInterfacedObject, IScreenCircle)
  private
    FPosition: TPointF;
    FdRadius: double;
    FclrBrush: TColor;
    FClrPen: TColor;
    FdVelocity: Double;

  protected
    function GetRadius: double;
    function GetBrushColor: TColor;
    function GetPenColor: TColor;
    function GetPosition: TPointF;
    function GetVelocity : Double;
    procedure Render(const ACanvas: TCanvas);
  public
    constructor Create(const APosition: TPOintF; const dRadius: double;
      clrBrush: TColor; clrPen: TColor; const dVelocity : Double);
  end;

  { TScreenCirclesList }

  TScreenCirclesList = class(TInterfacedObject, IScreenCirclesList)
  private
    FList: TObject;
  protected
    function getItem(const iIndex: integer): IScreenCircle;
    function GetCount: cardinal;
    procedure Clear;
    procedure Add(const intfScreenCircle: IScreenCircle);
    property Count: cardinal read GetCount;
    property Item[const iIndex: integer]: IScreenCircle read GetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  fgl;

type
  TInternalScreenCirclesList = specialize TFPGInterfacedObjectList<IScreenCircle>;

{ TScreenCirclesList }

function TScreenCirclesList.getItem(const iIndex: integer): IScreenCircle;
begin
  Result := TInternalScreenCirclesList(FList)[iIndex];
end;

function TScreenCirclesList.GetCount: cardinal;
begin
  Result := TInternalScreenCirclesList(FList).Count;
end;

procedure TScreenCirclesList.Clear;
begin
  TInternalScreenCirclesList(FList).Clear;
end;

procedure TScreenCirclesList.Add(const intfScreenCircle: IScreenCircle);
begin
  TInternalScreenCirclesList(FList).add(intfScreenCircle);
end;

constructor TScreenCirclesList.Create;
begin
  FList := TInternalScreenCirclesList.Create;
end;

destructor TScreenCirclesList.Destroy;
begin
  inherited Destroy;
end;

{ TScreenCircle }

function TScreenCircle.GetRadius: double;
begin
  Result := FdRadius;
end;

function TScreenCircle.GetBrushColor: TColor;
begin
  Result := FclrBrush;
end;

function TScreenCircle.GetPenColor: TColor;
begin
  Result := FClrPen;
end;

function TScreenCircle.GetPosition: TPointF;
begin
  Result := FPosition;
end;

function TScreenCircle.GetVelocity: Double;
begin
  RESULT := FdVelocity;
end;

procedure TScreenCircle.Render(const ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := FclrBrush;
  ACanvas.Pen.Color := FClrPen;
  ACanvas.Ellipse(round(FPosition.X - FdRadius),
    round(FPosition.Y - FdRadius),
    round(FPosition.X + FdRadius),
    round(FPosition.Y + FdRadius));
end;


constructor TScreenCircle.Create(const APosition: TPOintF; const dRadius: double;
  clrBrush: TColor; clrPen: TColor; const dVelocity : Double);
begin
  FPosition := APosition;
  FdRadius := dRadius;
  FclrBrush := clrBrush;
  FClrPen := clrPen;
  FdVelocity:= dVelocity;
end;

end.
