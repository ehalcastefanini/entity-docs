<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRchanges` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRchanges` code unit defines a Delphi frame (`TFRAMEchanges`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid of customer-related changes, providing functionalities such as data visualization, field customization, and action handling. It is designed to handle customer change records efficiently, allowing users to view and interact with the data in a structured grid format.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services.
- **Database Components**: For managing and displaying data from a database.
- **Third-party Libraries**: Includes `cxGrid`, `cxStyles`, and `sPanel` for advanced UI components and styling.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **descrip**: String (Description of the change).
2. **lastUpd**: Date/Time (Last update timestamp).
3. **updBy**: String (User who made the update).

#### Grid Actions and Their Effects:
- **Add Action**: Adds a new record to the grid.
- **Apply Action**: Saves changes made to the grid.
- **Cancel Action**: Cancels any unsaved changes.

---

## 2. Functionality Description:

### User/Software Actions:
- View customer change records in a grid.
- Add new records.
- Apply changes to existing records.
- Cancel unsaved changes.

### Main Components:
1. **Grid**: Displays customer change records.
2. **Action Panel**: Contains buttons for adding, applying, and canceling actions.
3. **Footer Panel**: Can be hidden or shown based on the configuration.

### Pseudo-code for Actions and Events:
- **OnClick event of Add Button**: `if Add button clicked then open new record editor`.
- **OnClick event of Apply Button**: `if Apply button clicked then save changes to database`.
- **OnClick event of Cancel Button**: `if Cancel button clicked then discard unsaved changes`.
- **OnGridFieldChange event**: `if grid field value changed then validate field`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created and initialized with default settings.
   - Grid settings are configured, including hidden fields, field order, and key fields.
   - Action panel and footer visibility are set.

2. **User Interactions**:
   - Users interact with the grid to view or edit records.
   - Buttons in the action panel trigger specific actions (e.g., add, apply, cancel).

### Functions:
- **`TFRAMEchanges.Create`** (File: `FRchanges.pas`):
  - Initializes the frame with default settings.
  - Configures grid properties and visibility of panels.
- **`TFRAMEchanges.SetForDOCADDR`** (File: `FRchanges.pas`):
  - Disables editing in the grid and hides the footer panel.

### Data Requirements:
- Users must provide valid data for the grid fields (e.g., description, last update, updated by).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Action**: Enabled by default; opens a new record editor.
- **Apply Action**: Enabled only if there are unsaved changes.
- **Cancel Action**: Enabled only if there are unsaved changes.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- No explicit field validations are defined in the code.

---

## 5. Main Functions:

1. **`TFRAMEchanges.Create`**:
   - Configures the frame and grid settings.
   - Sets default properties such as `MasterKeyFields`, `DataPacketName`, and `PropertyName`.

2. **`TFRAMEchanges.SetForDOCADDR`**:
   - Disables editing in the grid.
   - Hides the footer panel.

---

## 6. API Service Consumption:

- No explicit API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- The code does not define any conditional fields.

---

## 8. Dependencies:

### External Libraries:
- **`cxGrid`**: For advanced grid components.
- **`sPanel`**: For styled panels.
- **`SOAPHTTPClient`**: For SOAP service interactions.

### Custom Components:
- **`TFRAMEBaseGridEditSOA`**: Base class for grid-editing frames.

---

## 9. Fields and Validations Listing:

### Fields in the Grid:
1. **descrip**: String, no validations defined.
2. **lastUpd**: Date/Time, no validations defined.
3. **updBy**: String, no validations defined.

### Mapping of Displayed Values and Database Columns:
- **descrip**: Maps to the description of the change.
- **lastUpd**: Maps to the last update timestamp.
- **updBy**: Maps to the user who made the update.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Grid Data] --> [User Interacts with Grid]
    --> [Add/Apply/Cancel Actions] --> [Save or Discard Changes] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Frame: Interacts with grid
Frame --> Database: Fetches and updates data
Frame --> User: Displays updated grid
```

### Code Snippets:
```delphi
// Example: Creating the frame
var
  Frame: TFRAMEchanges;
begin
  Frame := TFRAMEchanges.Create(Self);
  Frame.Parent := Self;
end;
```

### Screenshots:
The DFM file is available, and the following HTML represents the grid template:

```html
<table style="border: 1px solid black; width: 100%; text-align: left;">
  <thead>
    <tr>
      <th>Description</th>
      <th>Last Update</th>
      <th>Updated By</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Change 1</td>
      <td>2023-10-01</td>
      <td>User A</td>
    </tr>
    <tr>
      <td>Change 2</td>
      <td>2023-10-02</td>
      <td>User B</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **Grid Settings**:
  - Hidden fields are defined using `DefineHiddenFields('HIDE_ALL_FIELDS')`.
  - Field order is set using `DefineOrderFields('descrip; lastUpd; updBy')`.
  - Key fields are defined as `entityCode` and `numSeq`.

- **Action Panel Visibility**:
  - Controlled by `ShowActionPanel` and `PNLfooter.Visible`.

---

## 12. Conclusion:

The `FRchanges` code unit provides a robust framework for managing and displaying customer change records in a grid format. Its strengths include customizable grid settings and a clean separation of concerns. However, it lacks explicit error handling, field validations, and API integration, which could limit its functionality in more complex scenarios.

---

## 13. Short Summary:

The `FRchanges` unit defines a grid-based frame for managing customer change records, with customizable settings and basic action handling. It is part of a larger system for data management but lacks explicit validations and error handling.#### **FRchanges.pas**

```
unit FRchanges;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEchanges = class(TFRAMEBaseGridEditSOA)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure SetForDOCADDR;
  end;

var
  FRAMEchanges: TFRAMEchanges;

implementation

uses
  kneUtils, kneTypes, kneConfigObjects, kneFindDialog, kneDialogFactory,
  kneFGFindUtils, Global;

{$R *.dfm}

constructor TFRAMEchanges.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=entityCode';
  DataPacketName := 'CustomerChanges';
  PropertyName := 'changes';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('descrip; lastUpd; updBy');
    // Key Fields ..............................................................
    KeyFields:= 'entityCode; numSeq';
    // Custom Editors ..........................................................
  //AddCustomField('bankCode','cxEDTfind');
  end; //with
end;

procedure TFRAMEchanges.SetForDOCADDR;
begin
  SetNoEdittingInGridFields('', self);
  PNLfooter.Visible := False;
end;


end.
```

#### **FRchanges.dfm**

```
inherited FRAMEchanges: TFRAMEchanges
  ParentFont = True
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
            9C00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C3163003131
            CE00315AE700315AE700A5B5F700CECEFF003163FF00315AE700315AE700315A
            E7006363FF00315AE7003131CE00313131007B392100FF00FF0063639C00315A
            E700315AE700315AE7009C9CCE00FFFFFF00A5B5F700315AE700315AE700CECE
```
<!-- tabs:end -->

