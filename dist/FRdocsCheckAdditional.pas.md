<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRdocsCheckAdditional`

## 1. Overview:

### Main Objective:
The `FRdocsCheckAdditional` unit defines a Delphi frame (`TFRAMEdocsCheckAdditional`) that extends a base grid editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid-based interface for editing and managing additional document checklist information. The frame provides functionalities such as adding, deleting, and editing records in a grid, with specific configurations for field visibility, order, and custom editors.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services.
- **Database Components**: For managing and displaying data from a database.
- **Third-party Libraries**: Includes `cxGrid`, `cxEditRepository`, and other DevExpress components for advanced UI features.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **docCd**: Hidden field, used as a key field.
2. **required**: Custom editor (`cxICBOrequired`), an image combo box with predefined options.
3. **otherInfo**: Custom editor (`cxMSKinfo`), a masked input field.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new record to the grid with default values.
2. **Delete**: Deletes the selected record from the grid.
3. **Edit**: Allows editing of specific fields in the grid, depending on the access mode.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add Record**: Users can add a new record to the grid with default values for specific fields.
2. **Edit Record**: Users can edit certain fields in the grid unless the access mode is set to "VIEW".
3. **Delete Record**: Users can delete a selected record from the grid.

### Main Components:
1. **Grid (`cxGrid`)**: Displays the data in a tabular format.
2. **Custom Editors**:
   - `cxICBOrequired`: Image combo box for the "required" field.
   - `cxMSKinfo`: Masked input for the "otherInfo" field.
3. **Action Panel**: Provides buttons for adding and deleting records.

### Pseudo-code for Actions and Events:
- **Add Button Click**:
  ```
  if add button clicked then
    create new record with default values
    set focus to the "required" field
  ```
- **After Scroll Event**:
  ```
  if dataset scrolls then
    if access mode is not "VIEW" then
      disable editing for specific fields
  ```
- **Set Access Mode**:
  ```
  if access mode is not "VIEW" then
    disable editing for specific fields
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created and configured with specific settings for the grid, including hidden fields, field order, and custom editors.
   - The action panel is made visible, and available actions are defined.
2. **User Interactions**:
   - Clicking the "Add" button triggers the `ACTaddExecute` method, which adds a new record with default values.
   - Scrolling through the dataset triggers the `CDStableAfterScroll` method, which adjusts field editing permissions based on the access mode.

### Required Data:
- **docCd**: The document code, inherited from the master dataset.
- **required**: Default value is "O" (Optional).
- **otherInfo**: No default value is provided.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Record**:
   - Preconditions: None.
   - Action: Adds a new record with default values for `docCd` and `required`.
2. **Edit Record**:
   - Preconditions: Access mode must not be "VIEW".
   - Action: Allows editing of specific fields.
3. **Delete Record**:
   - Preconditions: A record must be selected.
   - Action: Deletes the selected record.

### Available Filters:
No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- **docCd**: Inherited from the master dataset.
- **required**: Default is "O" (Optional).

### Field Validation and Conditions:
- **required**: Must be one of the predefined values ("O", "N", "M").
- **otherInfo**: No specific validation is defined.

---

## 5. Main Functions:

1. **`Create`**:
   - Configures the grid settings, including hidden fields, field order, and custom editors.
   - Sets up the action panel and available actions.

2. **`ACTaddExecute`**:
   - Adds a new record to the grid with default values for `docCd` and `required`.

3. **`CDStableAfterScroll`**:
   - Adjusts field editing permissions based on the access mode.

4. **`m_SetAccessMode`**:
   - Disables editing for specific fields if the access mode is not "VIEW".

---

## 6. API Service Consumption:

No explicit API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- **"Required" Field**:
  - The field is editable only when the access mode is not "VIEW".

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**:
   - `cxGrid`: For grid display.
   - `cxEditRepository`: For custom editors.
2. **SOAP Components**:
   - `SOAPHTTPClient`: For potential SOAP service interactions.

### Custom Components:
1. **`TFRAMEBaseGridEditSOA`**: The base class for the frame.
2. **`kneFRGridManager`**: Manages grid settings and configurations.

---

## 9. Fields and Validations Listing:

1. **docCd**:
   - Type: String.
   - Required: Yes.
   - Default: Inherited from the master dataset.
   - Validation: Not explicitly defined.

2. **required**:
   - Type: String (Image Combo Box).
   - Required: Yes.
   - Default: "O" (Optional).
   - Validation: Must be one of "O", "N", "M".

3. **otherInfo**:
   - Type: String (Masked Input).
   - Required: No.
   - Validation: Not explicitly defined.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Grid Settings]
   --> [User Interaction] --> [Add/Edit/Delete Record] --> [End]
```

### Sequence Diagram:
```plaintext
User --> [Add Button] --> [ACTaddExecute] --> [Add Record to Grid]
User --> [Scroll Dataset] --> [CDStableAfterScroll] --> [Adjust Field Permissions]
```

### Code Snippets:
```delphi
procedure TFRAMEdocsCheckAdditional.ACTaddExecute(Sender: TObject);
begin
  inherited;
  CDStable.FieldByName('docCd').AsString := MasterSource.Dataset.FieldByname('docCd').AsString;
  CDStable.FieldByName('Required').AsString := 'O';
  SetNoEdittingInGridFields('Required;otherInfo', self);
  SetFocusinFieldGrid('Required', cxDBG, cxDBVtable);
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Grid Settings**:
  - Hidden fields: `docCd`.
  - Field order: `required;otherInfo`.
  - Custom editors: `required` (Image Combo Box), `otherInfo` (Masked Input).

- **Access Mode**:
  - Editing is disabled for specific fields when the access mode is "VIEW".

---

## 12. Conclusion:

The `FRdocsCheckAdditional` unit provides a robust framework for managing a grid-based interface for document checklist information. It is well-structured and leverages advanced UI components for customization. However, the lack of explicit error handling and validation logic may limit its robustness in certain scenarios.

---

## 13. Short Summary:

The `FRdocsCheckAdditional` unit defines a grid-based interface for managing document checklist information, with features for adding, editing, and deleting records. It uses DevExpress components for customization and enforces access-based field editing.#### **FRdocsCheckAdditional.pas**

```
unit FRdocsCheckAdditional;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEdocsCheckAdditional = class(TFRAMEBaseGridEditSOA)
    cxEDTfindVehicleType: TcxEditRepositoryButtonItem;
    cxICBOrequired: TcxEditRepositoryImageComboBoxItem;
    cxMSKinfo: TcxEditRepositoryMaskItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableAfterScroll(DataSet: TDataSet);
  private
    procedure m_SetAccessMode(Sender: TObject;var pv_state: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure SetKeyEditing(const EditKey: Boolean); override;
  end;

var
  FRAMEdocsCheckAdditional: TFRAMEdocsCheckAdditional;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, Global,
  kneTypes, kneFindDialog, kneDialogFactory,
  kneConfigObjects, kneFREditSOA;

{$R *.dfm}

constructor TFRAMEdocsCheckAdditional.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'docCd';
  DataPacketName := 'CheckListDocInfo';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'infos';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('docCd');
    // Ordem Campos ............................................................
    DefineOrderFields('required;otherInfo');
    // Key Fields ..............................................................
    KeyFields:= 'docCd;otherInfo';
    // Custom Editors ..........................................................
    AddCustomField('required','cxICBOrequired');
    AddCustomField('otherInfo','cxMSKinfo');
  end; //with

  // Atribui��o dos eventos dos Finds  
//  cxEDTfindVehicleType.Properties.OnButtonClick := m_FindVehicleType;
  OnSetAccessMode := m_SetAccessMode;
end;

procedure TFRAMEdocsCheckAdditional.m_SetAccessMode(Sender: TObject;var pv_state: Boolean);
begin
  if (accessMode <> 'VIEW')  then
    SetNoEdittingInGridFields('Required;otherInfo', self);
end;


procedure TFRAMEdocsCheckAdditional.ACTaddExecute(Sender: TObject);
begin
  inherited;
  CDStable.FieldByName('docCd').AsString := MasterSource.Dataset.FieldByname('docCd').AsString;
  CDStable.FieldByName('Required').AsString := 'O';
  SetNoEdittingInGridFields('Required;otherInfo', self);
  SetFocusinFieldGrid('Required', cxDBG, cxDBVtable);
end;

procedure TFRAMEdocsCheckAdditional.CDStableAfterScroll(DataSet: TDataSet);
begin
  inherited;
  if (accessMode <> 'VIEW')  then
    SetNoEdittingInGridFields('Required;otherInfo', self);
end;
```

#### **FRdocsCheckAdditional.dfm**

```
inherited FRAMEdocsCheckAdditional: TFRAMEdocsCheckAdditional
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindVehicleType: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
    object cxICBOrequired: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'Optional'
          ImageIndex = 0
          Value = 'O'
        end
        item
          Description = 'No'
          Value = 'N'
        end
        item
          Description = 'Mandatory'
          Value = 'M'
        end>
    end
    object cxMSKinfo: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
    end
  end
end
```
<!-- tabs:end -->

