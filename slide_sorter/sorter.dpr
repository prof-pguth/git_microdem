program sorter;

uses
  System.StartUpCopy,
  FMX.Forms,
  slider_sorter_form in 'slider_sorter_form.pas' {SlideSorterForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSlideSorterForm, SlideSorterForm);
  Application.Run;
end.
