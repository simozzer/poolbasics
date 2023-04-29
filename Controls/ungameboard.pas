unit unGameBoard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, unBoardRenderer, Graphics;

type

  { TCaromGameBoard }

  TCaromGameBoard = class(TCustomControl)
  private
    FRenderer: TBoardRenderer;
    FiWidth: integer;
    FiHeight: integer;
    function GetBoardCanvas: TCanvas;

    procedure SetWidth(const iWidth: integer);
    procedure SetHeight(const iHeight: integer);
  protected

  public
    procedure Render;
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BoardCanvas: TCanvas read GetBoardCanvas;
    property Width: integer read FiWidth write SetWidth;
    property Height: integer read FiHeight write SetHeight;
    property OnMouseDown;

  end;

implementation


const
  DEFAULT_WIDTH = 600;
  DEFAULT_HEIGHT = 600;


{ TCaromGameBoard }

procedure TCaromGameBoard.Paint;
begin
  inherited Paint;
  Canvas.Draw(0, 0, FRenderer.Bitmap);
end;

procedure TCaromGameBoard.SetWidth(const iWidth: integer);
begin
  if (iWidth = FiWidth) then exit;
  FiWidth := iWidth;
  FRenderer.Width := iWidth;
  inherited Width := iWidth;
  invalidate;
end;

procedure TCaromGameBoard.SetHeight(const iHeight: integer);
begin
  if (iHeight = FiHeight) then exit;
  FiHeight := iHeight;
  FRenderer.Height := iHeight;
  inherited Height := iHeight;
  invalidate;
end;

function TCaromGameBoard.GetBoardCanvas: TCanvas;
begin
  Result := FRenderer.Bitmap.Canvas;
end;

procedure TCaromGameBoard.Render;
begin
  FRenderer.Render;
end;

constructor TCaromGameBoard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRenderer := TBoardRenderer.Create(DEFAULT_WIDTH, DEFAULT_HEIGHT, clCream);
  Height:= DEFAULT_HEIGHT;
  Width:= DEFAULT_WIDTH;
  Render;
end;

destructor TCaromGameBoard.Destroy;
begin
  inherited Destroy;
  FRenderer.Free;
end;

end.
