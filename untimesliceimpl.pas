unit unTimesliceImpl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unHelperInterfaces;

{ TTimeslice }
type

  TTimeslice = class(TInterfacedObject, ITimeslice)
  private
    FdStartTime: double;
    FdEndTime: double;
    FlstPathParts: IPathPartList;
  protected
    function GetStartTime: double;
    function GetEndTime: double;
    function GetPathParts: IPathPartList;
    procedure SetStartTime(const dStartTime: double);
    procedure SetEndTime(const dEndTime: double);
    function ToString(): string; override;
  public
    constructor Create(const APathParts: IPathPartList);
    destructor Destroy; override;
  end;




  { TTimesliceList }

  TTimesliceList = class(TInterfacedObject, ITimesliceList)
  private
    FList: TObject;
  protected
    function getItem(const iIndex: integer): ITimeslice;
    function GetCount: cardinal;
    function indexOf(Const intfTimeSlice: ITimeslice): Integer;
    procedure Clear;
    procedure Add(const intfTimeslice: ITimeslice);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses FGL;

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

function TTimesliceList.indexOf(const intfTimeSlice: ITimeslice): Integer;
begin
  Result := TInternalTimesliceList(FList).IndexOf(intfTimeSlice);
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

function TTimeslice.GetPathParts: IPathPartList;
begin
  Result := FlstPathParts;
end;

procedure TTimeslice.SetStartTime(const dStartTime: double);
begin
  FdStartTime := dStartTime;
end;

procedure TTimeslice.SetEndTime(const dEndTime: double);
begin
  FdEndTime := dEndTime;
end;

function TTimeslice.ToString: string;
var
  sl: TStrings;
  I: integer;
begin
  sl := TStringList.Create;
  try
    sl.Add(Format('Start Time: %f, End Time: %f', [FdStartTime, FdEndTime]));
    for i := 0 to pred(FlstPathParts.Count) do
    begin
      sl.Add('    (' + IntToStr(i) + ')' + FlstPathParts[i].ToString());
    end;
    Result := sl.CommaText;
  finally
    sl.Free;
  end;
end;

constructor TTimeslice.Create(const APathParts: IPathPartList);
begin
  FlstPathParts := APathParts;
  FdEndTime := 0;
  FdStartTime := 0;
end;

destructor TTimeslice.Destroy;
begin
  FlstPathParts := nil;
  inherited Destroy;
end;

end.
