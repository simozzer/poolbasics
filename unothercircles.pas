unit unOtherCircles;

{$mode ObjFPC}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Graphics, Fgl, unHelperInterfaces;

type

  { TBaseCircle }

  TBaseCircle = class(TInterfacedObject, ICircle, IIdentity)
  private
    FdRadius: double;
    FdMass: double;
    FclrBrush: TColor;
    FclrPen: TColor;
    FiID: cardinal;

    {$IFDEF DEBUG}
    FsLabel: string;
    {$ENDIF}
  protected
    function GetRadius: double;
    function GetBrushColor: TColor;
    procedure SetBrushColor(const clr: TColor);
    function GetPenColor: TColor;
    procedure SetPenColor(const clr: TColor);
    function GetMass: double;
    function GetId: cardinal;
    function ToString: ansistring; override;

    {$IFDEF DEBUG}
    function GetLabel: string;
    procedure SetLabel(const sLabel: string);
    {$ENDIF}
  public
    constructor Create(const dRadius, dMass: double);
  end;

  { TCirclesList }

  TCirclesList = class(TInterfacedObject, ICirclesList)
  private
    FList: TObject;
  protected
    function GetItem(const iIndex: integer): ICircle;
    function GetCount: cardinal;
    procedure Clear;
    procedure Add(const intfCircle: ICircle);
  public
    constructor Create;
    destructor Destroy; override;
  end;



implementation

type
  // Contains a list of ICircle
  TInternalCirclesList = specialize TFPGInterfacedObjectList<ICircle>;

var
  miCircleIdSequence: cardinal;



{ TCirclesList }

function TCirclesList.GetItem(const iIndex: integer): ICircle;
begin
  Result := TInternalCirclesList(FList)[iIndex];
end;

function TCirclesList.GetCount: cardinal;
begin
  Result := TInternalCirclesList(FList).Count;
end;

procedure TCirclesList.Clear;
begin
  TInternalCirclesList(FList).Clear;
end;

procedure TCirclesList.Add(const intfCircle: ICircle);
begin
  TInternalCirclesList(FList).Add(intfCircle);
end;

constructor TCirclesList.Create;
begin
  FList := TInternalCirclesList.Create;
end;

destructor TCirclesList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ TBaseCircle }

function TBaseCircle.GetRadius: double;
begin
  Result := FdRadius;
end;

function TBaseCircle.GetBrushColor: TColor;
begin
  Result := FclrBrush;
end;

procedure TBaseCircle.SetBrushColor(const clr: TColor);
begin
  FclrBrush := clr;
end;

function TBaseCircle.GetPenColor: TColor;
begin
  Result := FclrPen;
end;

procedure TBaseCircle.SetPenColor(const clr: TColor);
begin
  FclrPen := clr;
end;


function TBaseCircle.GetMass: double;
begin
  Result := FdMass;
end;


function TBaseCircle.GetId: cardinal;
begin
  Result := FiId;
end;

function TBaseCircle.ToString: ansistring;
begin
  {$IFDEF DEBUG}
  Result := Format('%s, Radius: %f, Mass: %f', [FsLabel, FdRadius, FdMass]);
  {$ELSE}
  Result := Format('Radius: %f, Mass: %f', [FdRadius, FdMass]);
  {$ENDIF}
end;

{$IFDEF DEBUG}
function TBaseCircle.GetLabel: string;
begin
  Result := FsLabel;
end;

procedure TBaseCircle.SetLabel(const sLabel: string);
begin
  FsLabel := sLabel;
end;

{$ENDIF}

constructor TBaseCircle.Create(const dRadius, dMass: double);
begin
  FdRadius := dRadius;
  FclrBrush := clWhite;
  FclrPen := clBlack;
  FdMass := dMass;
  miCircleIdSequence := miCircleIdSequence + 1;
  FiID := miCircleIdSequence;

  {$IFDEF DEBUG}
  FsLabel := '';
  {$ENDIF}
end;


initialization
  miCircleIdSequence := 1;

end.
