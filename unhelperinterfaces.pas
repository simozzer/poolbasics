unit unHelperInterfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  IBasicLogger = interface
    ['{2D3C8D9A-8E26-4B28-A66F-74F1BCCFFA54}']
    procedure LogMessage(const sMessage: string);
  end;

  IBasicLoggerClient = interface(IBasicLogger)
    ['{8ECB82CB-DD04-4E7B-8F3C-7188E320A28A}']
    procedure SetLogger(const intfLogger: IBasicLogger);
  end;



implementation

end.
