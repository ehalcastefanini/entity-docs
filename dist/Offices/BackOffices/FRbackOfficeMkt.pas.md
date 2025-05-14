<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar dados relacionados ao mercado no back-office. Ele fornece uma interface de grade (grid) para exibir, editar e gerenciar informações de mercado, permitindo ações como adicionar e excluir registros. O objetivo principal é facilitar a manipulação de dados de mercado de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes VCL:** Incluindo `TcxGrid`, `TcxGridDBTableView`, e `TcxEditRepositoryButtonItem` para criar a interface gráfica.
  - **SOAP:** Utilizado para comunicação com serviços externos, como o `CustomerMarketServiceUtils`.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `market` (string): Código do mercado.
      - `mktDescr` (string): Descrição do mercado.
    - **Ações da Grade e seus Efeitos:**
      - **Adicionar (`add`):** Adiciona um novo registro.
      - **Excluir (`delete`):** Remove o registro selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar um novo mercado.
  - Excluir um mercado existente.
  - Editar valores diretamente na grade.
  - Pesquisar mercados por código ou descrição.

* **Componentes Principais:**
  - `TcxGrid`: Exibe os dados em formato de tabela.
  - `TcxEditRepositoryButtonItem`: Botão para ações específicas, como buscar mercados.
  - `TFRAMEBaseGridEditSOA`: Classe base que fornece funcionalidades padrão para edição em grade.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `if botão adicionar clicado then executar função ACTaddExecute`.
  - Evento `OnEditValueChanged` na grade: `if valor da célula alterado then validar e salvar alteração`.
  - Evento `OnButtonClick` do botão de busca: `if botão buscar clicado then executar função m_FindMarket`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente:
     - Configuração de campos e propriedades da grade.
     - Definição de ações disponíveis (`add`, `delete`).
  2. Interação do usuário:
     - O usuário pode adicionar, excluir ou editar registros diretamente na grade.
     - A busca por mercados é realizada ao clicar no botão de busca.
  3. Funções executadas:
     - `ACTaddExecute`: Adiciona um novo registro.
     - `m_FindMarket`: Realiza a busca de mercados.
     - `VerifyDataBeforeSave`: Verifica a validade dos dados antes de salvar.

* **Dados Necessários:**
  - Código do mercado (`market`).
  - Descrição do mercado (`mktDescr`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se um registro estiver selecionado.
  - **Editar:** Disponível para campos não somente leitura.

* **Filtros Disponíveis:**
  - Filtros não explicitamente definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se o campo `market` estiver vazio.
  - "Valor inválido" se o valor inserido não for aceito.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - Campo `market`: Obrigatório.
  - Campo `mktDescr`: Não obrigatório.

---

## 5. Funções Principais:

* **ACTaddExecute:** Adiciona um novo registro à grade.
* **m_FindMarket:** Realiza a busca de mercados com base no código ou descrição.
* **VerifyDataBeforeSave:** Verifica se os dados estão válidos antes de salvar.
* **SetFieldRequiredState:** Define se um campo é obrigatório ou não.

---

## 6. Consumo de Serviços API:

* **Serviço:** `CustomerMarketServiceUtils`.
* **Endpoint:** Não especificado no código.
* **Dados Enviados:** Não especificado no código.
* **Dados Recebidos:** Não especificado no código.
* **Propósito:** Gerenciar dados de mercado.
* **Tratamento de Erros:** Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explicitamente definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `cxGrid`, `cxEditRepository`: Para criação da interface gráfica.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para edição em grade.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `market` (string, obrigatório).
  - `mktDescr` (string, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `market` → Coluna `market`.
  - `mktDescr` → Coluna `mktDescr`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEbackOfficeMkt := TFRAMEbackOfficeMkt.Create(Self);
  FRAMEbackOfficeMkt.ACTaddExecute(Self);
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width: 100%; border: 1px solid black;">
    <thead>
      <tr>
        <th style="width: 100px;">Market</th>
        <th style="width: 150px;">Description</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>001</td>
        <td>Market A</td>
      </tr>
      <tr>
        <td>002</td>
        <td>Market B</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de campos e propriedades da grade:
  ```delphi
  DefineHiddenFields('HIDE_ALL_FIELDS');
  DefineOrderFields('market;mktDescr');
  ```

* Configuração de ações disponíveis:
  ```delphi
  AvailableActions := 'add;delete';
  ```

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar dados de mercado no back-office. Ele é bem estruturado e utiliza componentes modernos para criar uma interface intuitiva. No entanto, faltam detalhes sobre a integração com serviços externos e validações mais robustas.

---

## 13. Resumo Curto:

O código implementa uma interface de grade para gerenciar dados de mercado no back-office, permitindo adicionar, excluir e editar registros. Ele utiliza componentes VCL e SOAP para comunicação com serviços externos, mas carece de detalhes sobre validações e integração com APIs.#### **FRbackOfficeMkt.pas**

```
unit FRbackOfficeMkt;

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
  TFRAMEbackOfficeMkt = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_OnSetAccessMode(Sender: TObject; var pv_value: Boolean);
    function VerifyDataBeforeSave: Boolean;
    procedure m_ConfigFields(const Dataset: TDataSet;
      const Datapacket: string);
    function SetFieldRequiredState(const pv_Field: TField;
      pv_State: Boolean): Boolean;
    procedure m_FindMarket(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEbackOfficeMkt: TFRAMEbackOfficeMkt;

const
  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneFGControlsUtils, Global, kneUtils, kneFindDialog,
  kneDialogFactory, kneFGFindUtils,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

constructor TFRAMEbackOfficeMkt.Create(AOwner: TComponent);
var
  lv_form:  TCustomForm;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'backoffice';
  DataPacketName := 'BackOfficeMkt';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'backOffMkt';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'add;delete';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
    DefineOrderFields('market;mktDescr');

    // Key Fields ..............................................................
//    KeyFields:= '';

    // Custom Editors ..........................................................
    AddCustomField('market','cxEDTfindMarket');
    UseColsBestFit := False;
  end; //with

  OnSetAccessMode := m_OnSetAccessMode;
  cxEDTfindMarket.Properties.OnButtonClick := m_FindMarket;

  lv_form := TkneFGControlsUtils.fg_GetOwnerForm(Self);
end;

procedure TFRAMEbackOfficeMkt.m_OnSetAccessMode(Sender: TObject;var pv_value: Boolean);
begin
  inherited;

  SetColsWidthInGrid(gc_ColsWith, cxDBVtable);

  if (not assigned(CDStable)) or (not CDStable.Active) then 
    exit;  

  CDStable.FieldByName('market').ReadOnly := False;
  SetFieldRequiredState(CDStable.FieldByName('market'), True);
```

#### **FRbackOfficeMkt.dfm**

```
inherited FRAMEbackOfficeMkt: TFRAMEbackOfficeMkt
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindMarket: TcxEditRepositoryButtonItem
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
    end
  end
end
```
<!-- tabs:end -->

