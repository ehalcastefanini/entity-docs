<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustomerGroupLink` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcustomerGroupLink` code unit defines a specialized frame (`TFRAMEcustomerGroupLink`) that inherits from `TFRAMEBaseGridEditSOA`. Its primary purpose is to manage and display a grid-based interface for linking customer groups. This frame is designed to handle data visualization and interaction with a database, specifically for managing customer group links. It provides functionalities such as defining read-only fields, hidden fields, field order, and key fields for the grid.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the user interface and managing components.
- **SOAP/HTTP Client**: For potential integration with web services.
- **Database Components**: For interacting with the database (e.g., `DB`, `DBClient`).
- **cxGrid**: A component for creating and managing grid-based data displays.
- **Custom Libraries**: Includes `kneFRGridEditSOA`, `kneTypes`, and `kneFRGridManager` for extended functionality.

### Form Type:
This code represents a **grid display**.

#### Grid Columns and Their Types:
- `mill`: Read-only field.
- `millGroup`: Read-only field.
- `descrip`: Read-only field.
- `stat`: Read-only field.
- `millDtime`: Read-only field.

#### Grid Actions and Their Effects:
- **Read-Only Fields**: Prevents editing of specific fields.
- **Hidden Fields**: Hides all fields by default (`HIDE_ALL_FIELDS`).
- **Field Order**: Specifies the display order of fields in the grid.
- **Key Fields**: Defines `group` as the key field for data identification.

---

## 2. Functionality Description:

### User/Software Actions:
- Display a grid with customer group links.
- Configure grid settings such as read-only fields, hidden fields, and field order.
- Interact with the grid to view data (editing is disabled).

### Main Components:
- **`TFRAMEcustomerGroupLink`**: The main frame class that encapsulates the grid and its settings.
- **`cxGrid`**: The grid component for displaying data.
- **`cxGridDBTableView`**: A database-aware table view for the grid.

### Pseudo-Code for Actions and Events:
- **Grid Initialization**:
  ```
  if frame is created then
    set MasterKeyFields to 'groupCode=group'
    set DataPacketName to 'LnkGroup'
    set PropertyName to 'links'
    set FrameType to frtDetail
    configure grid settings
  ```
- **Grid Settings**:
  ```
  if grid settings are defined then
    set read-only fields to 'mill; millGroup; descrip; stat; millDtime'
    hide all fields
    set field order to 'mill; millGroup; descrip; stat; millDtime'
    set key fields to 'group'
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `TFRAMEcustomerGroupLink` frame is created.
   - The `Create` constructor initializes the frame and sets up grid properties.
2. **Grid Configuration**:
   - Read-only fields, hidden fields, field order, and key fields are defined.
3. **User Interaction**:
   - Users can view the grid but cannot edit fields due to the read-only settings.

### Data Requirements:
- The grid requires data with the following fields: `mill`, `millGroup`, `descrip`, `stat`, `millDtime`, and `group`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Grid Display**: The grid is displayed with predefined settings.
- **Read-Only Fields**: Fields are non-editable.
- **Hidden Fields**: All fields are hidden by default unless explicitly shown.

### Available Filters:
- No filters are explicitly defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- No explicit field validations or conditions are defined in the code.

---

## 5. Main Functions:

### `TFRAMEcustomerGroupLink.Create`:
- **Purpose**: Initializes the frame and configures grid settings.
- **Logic**:
  - Sets `MasterKeyFields`, `DataPacketName`, `PropertyName`, and `FrameType`.
  - Configures grid settings (read-only fields, hidden fields, field order, and key fields).

---

## 6. API Service Consumption:

- No external API service calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are defined in the provided code.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid-based data display.
- **SOAPHTTPClient**: For potential SOAP-based web service integration.
- **DBClient**: For database interaction.

### Custom Components:
- **kneFRGridEditSOA**: Base class for grid editing functionality.
- **kneFRGridManager**: For managing grid settings.

---

## 9. Fields and Validations Listing:

### Fields:
- `mill` (type: string, read-only).
- `millGroup` (type: string, read-only).
- `descrip` (type: string, read-only).
- `stat` (type: string, read-only).
- `millDtime` (type: datetime, read-only).
- `group` (type: string, key field).

### Mapping:
- Displayed values are mapped to database columns with the same names.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Create Frame] --> [Initialize Grid Settings] --> [Display Grid]
```

### Sequence Diagram:
```plaintext
User --> Frame: Create
Frame --> Grid: Configure Settings
Grid --> User: Display Data
```

### Code Snippets:
```delphi
var
  Frame: TFRAMEcustomerGroupLink;
begin
  Frame := TFRAMEcustomerGroupLink.Create(Self);
  Frame.Parent := Self;
  Frame.Show;
end;
```

### Screenshots:
The DFM file is provided, so the following HTML represents the grid:
```html
<table style="border: 1px solid black; width: 100%;">
  <thead>
    <tr>
      <th>mill</th>
      <th>millGroup</th>
      <th>descrip</th>
      <th>stat</th>
      <th>millDtime</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Example Mill</td>
      <td>Group A</td>
      <td>Description</td>
      <td>Active</td>
      <td>2023-10-01</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **Grid Settings**: The `GridSettings` block defines read-only fields, hidden fields, field order, and key fields.
- **Metadata Configuration**: `MasterKeyFields`, `DataPacketName`, and `PropertyName` are critical for linking the grid to the data source.

---

## 12. Conclusion:

The `FRcustomerGroupLink` code unit provides a robust framework for managing and displaying customer group links in a grid format. Its strengths lie in its modularity and configurability. However, it lacks explicit error handling, field validations, and user interaction features beyond data display.

---

## 13. Short Summary:

The `FRcustomerGroupLink` unit defines a grid-based interface for managing customer group links, with configurable settings for read-only fields, hidden fields, and field order. It is part of a larger system for database interaction and data visualization.#### **FRcustomerGroupLink.pas**

```
unit FRcustomerGroupLink;

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
  TFRAMEcustomerGroupLink = class(TFRAMEBaseGridEditSOA)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEcustomerGroupLink: TFRAMEcustomerGroupLink;

implementation

uses
  kneTypes, Global;

{$R *.dfm}

{ TFRAMEcustomerGroupLink }

constructor TFRAMEcustomerGroupLink.Create(AOwner: TComponent);
var
  lv_form:  TCustomForm;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'groupCode=group';
  DataPacketName := 'LnkGroup';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'links';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('mill; millGroup; descrip; stat; millDtime');
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('mill; millGroup; descrip; stat; millDtime');
    // Key Fields ..............................................................
    KeyFields:= 'group';
    // Custom Editors ..........................................................
//    AddCustomField('marketCode','cxEDTfindMarket');
//    UseColsBestFit := False;
  end; //with

end;


end.
```

#### **FRcustomerGroupLink.dfm**

```
inherited FRAMEcustomerGroupLink: TFRAMEcustomerGroupLink
  inherited cxDBG: TcxGrid
    Enabled = False
    inherited cxDBVtable: TcxGridDBTableView
      Styles.Content = cxSTLReadOnly
    end
  end
end
```
<!-- tabs:end -->

